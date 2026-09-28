/**
 * Canvas capture helpers for query_ui / MCP visual content extraction.
 *
 * Captures every visible canvas (2D or WebGL), image, and SVG inside an
 * element, composited into one PNG as laid out on screen (e.g. a WebGL
 * canvas with a 2D overlay, or a card with several plots).
 */

/**
 * Copy a canvas (2D or WebGL) onto a temporary 2D canvas.
 * For WebGL canvases without preserveDrawingBuffer, drawImage may return
 * blank pixels.  In that case we fall back to readPixels.  A WebGL or
 * WebGPU canvas that still reads blank is marked `gpuBlank`.
 * @param {HTMLCanvasElement} source
 * @returns {HTMLCanvasElement} a 2D canvas with the source content
 */
export function canvasTo2D(source) {
  const w = source.width;
  const h = source.height;
  const tmp = document.createElement('canvas');
  tmp.width = w;
  tmp.height = h;
  const ctx = tmp.getContext('2d');

  // Fast path: drawImage
  ctx.drawImage(source, 0, 0);

  // Check if the result is blank (WebGL buffer may have been cleared)
  const imageData = ctx.getImageData(0, 0, w, h);
  const data = imageData.data;
  let isBlank = true;
  // Sample every 50th pixel for speed
  for (let i = 3; i < data.length; i += 200) {
    if (data[i] !== 0) { isBlank = false; break; }
  }

  if (!isBlank) return tmp;

  // Fallback: try reading from WebGL context directly.
  // getContext() returns the *existing* context if one was already created
  // with the same type, or null otherwise. Try both types.
  let gl = null;
  try { gl = source.getContext('webgl2'); } catch (e) { /* ignore */ }
  if (!gl) {
    try { gl = source.getContext('webgl'); } catch (e) { /* ignore */ }
  }
  if (!gl) {
    // A WebGPU canvas (e.g. three.js WebGPURenderer, used by threeBrain)
    // cannot be read back either once its frame is shown
    let gpu = null;
    try { gpu = source.getContext('webgpu'); } catch (e) { /* ignore */ }
    if (gpu) tmp.gpuBlank = true;
    return tmp;
  }

  const pixels = new Uint8Array(w * h * 4);
  gl.readPixels(0, 0, w, h, gl.RGBA, gl.UNSIGNED_BYTE, pixels);

  // Check if readPixels actually got data
  let hasData = false;
  for (let i = 3; i < pixels.length; i += 200) {
    if (pixels[i] !== 0) { hasData = true; break; }
  }
  if (!hasData) {
    // A WebGL canvas that is not being redrawn: its pixels are gone
    tmp.gpuBlank = true;
    return tmp;
  }

  // readPixels gives bottom-to-top rows; flip vertically
  const flipped = ctx.createImageData(w, h);
  const rowSize = w * 4;
  for (let y = 0; y < h; y++) {
    const srcOffset = (h - y - 1) * rowSize;
    const dstOffset = y * rowSize;
    flipped.data.set(pixels.subarray(srcOffset, srcOffset + rowSize), dstOffset);
  }
  ctx.putImageData(flipped, 0, 0);
  return tmp;
}

/**
 * Load an <svg> element as an <img> of size w x h (async). Styles from
 * stylesheets are not carried over; inline attributes and styles are.
 * @param {SVGElement} svgEl
 * @returns {Promise<HTMLImageElement|null>}
 */
function svgToImage(svgEl, w, h) {
  return new Promise((resolve) => {
    try {
      const clone = svgEl.cloneNode(true);
      clone.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
      clone.setAttribute('width', w);
      clone.setAttribute('height', h);

      const svgString = new XMLSerializer().serializeToString(clone);
      const img = new Image();
      img.onload = () => resolve(img);
      img.onerror = () => resolve(null);
      img.src = 'data:image/svg+xml;charset=utf-8,' + encodeURIComponent(svgString);
    } catch (e) {
      resolve(null);
    }
  });
}

/** Split a data URL into { image_data, image_type }, or null. */
function splitDataURL(dataUrl) {
  const parts = (dataUrl || '').split(',');
  const mime = (parts[0] || '').replace(/^data:/, '').replace(/;base64$/, '') || 'image/png';
  if (!parts[1]) return null;
  return { image_data: parts[1], image_type: mime };
}

// Visual elements smaller than this (in px^2 on screen) are icons, not content
const MIN_VISUAL_AREA = 32 * 32;
// The longest side of the returned image, in px
const MAX_IMAGE_SIDE = 2000;

/** The size an element would have with no layout: its buffer or attributes. */
function intrinsicSize(node) {
  if (node.tagName === 'CANVAS') {
    return { w: node.width, h: node.height };
  }
  if (node.tagName === 'IMG') {
    return {
      w: node.naturalWidth || parseFloat(node.getAttribute('width')) || 0,
      h: node.naturalHeight || parseFloat(node.getAttribute('height')) || 0
    };
  }
  // SVG: width/height attributes, else the viewBox
  let w = parseFloat(node.getAttribute('width')) || 0;
  let h = parseFloat(node.getAttribute('height')) || 0;
  const vb = node.viewBox && node.viewBox.baseVal;
  if ((!w || !h) && vb && vb.width && vb.height) {
    w = w || vb.width;
    h = h || vb.height;
  }
  return { w, h };
}

/** Images that can be drawn without tainting the canvas. */
function isDrawableImg(img) {
  const src = img.currentSrc || img.getAttribute('src') || '';
  if (!src) return false;
  if (src.startsWith('data:') || src.startsWith('blob:')) return true;
  try {
    return new URL(src, window.location.href).origin === window.location.origin;
  } catch (e) {
    return false;
  }
}

/**
 * The z-index that decides whether `node` paints over its siblings: from
 * the nearest positioned ancestor (within `root`) that sets one.
 */
function stackLevel(node, root) {
  for (let p = node; p && p !== root; p = p.parentElement) {
    const cs = getComputedStyle(p);
    if (cs.position !== 'static' && cs.zIndex !== 'auto') {
      return parseInt(cs.zIndex, 10) || 0;
    }
  }
  return 0;
}

/** The first non-transparent background color at or above `el`. */
function effectiveBackground(el) {
  for (let p = el; p && p.nodeType === 1; p = p.parentElement) {
    const bg = getComputedStyle(p).backgroundColor;
    if (bg && bg !== 'transparent' && !/rgba\(.*,\s*0\)$/.test(bg)) return bg;
  }
  return '#ffffff';
}

/**
 * Copy the canvases in `items` to 2D canvases (`item.snapshot`) in the
 * next animation frame: a WebGL canvas without `preserveDrawingBuffer`, or
 * a WebGPU canvas (e.g. the threeBrain viewer), is cleared once shown, and
 * holds its pixels only right after the page renders a frame. Hidden frames may run no
 * animation frames, so the copy also runs after `timeoutMs`.
 */
function snapshotCanvases(items, timeoutMs = 200) {
  return new Promise((resolve) => {
    let done = false;
    const run = () => {
      if (done) return;
      done = true;
      items.forEach((item) => {
        if (item.type !== 'canvas') return;
        try {
          item.snapshot = canvasTo2D(item.el);
        } catch (e) {
          // e.g. a tainted canvas: leave it out
          item.snapshot = null;
        }
      });
      resolve();
    };
    requestAnimationFrame(run);
    setTimeout(run, timeoutMs);
  });
}

/**
 * Draw one visual element onto `ctx` at (x, y, w, h); resolves false when
 * there was nothing to draw (canvases need `snapshotCanvases()` first).
 */
async function drawVisual(ctx, item, x, y, w, h) {
  let source = null;
  if (item.type === 'canvas') {
    source = item.snapshot;
  } else if (item.type === 'img') {
    source = item.el;
  } else {
    source = await svgToImage(item.el, w, h);
  }
  if (!source) return false;
  ctx.drawImage(source, x, y, w, h);
  return true;
}

/**
 * Capture the visual content of `el` (async): every visible <canvas>,
 * drawable <img>, and outermost <svg> in it (or `el` itself), composited
 * into one PNG as laid out on screen, in paint order, on the element's
 * background color, and cropped to the area they cover. Tiny elements
 * (icons), elements with no box, and (when anything else is shown)
 * elements hidden with `visibility` are left out.
 *
 * When `el` has no layout (inside a hidden tab or a collapsed card, e.g.
 * an output that `shiny_output_result` rendered there), positions are
 * unknown, so the largest element is returned alone.
 *
 * Resolves with { image_data, image_type, note } or null when there is no
 * visual content (the caller then falls back to HTML).
 * @param {Element} el
 */
export async function captureVisualContent(el) {
  const selector = 'canvas, img, svg';
  const nodes = el.matches(selector) ? [el] : Array.from(el.querySelectorAll(selector));
  const containerRect = el.getBoundingClientRect();
  const laidOut = containerRect.width > 0 || containerRect.height > 0;

  // --- gather candidates -----------------------------------------------------
  const candidates = [];
  nodes.forEach((node, order) => {
    const tag = node.tagName.toUpperCase();
    // An <svg> inside another <svg> is drawn with its parent
    if (tag === 'SVG' && node.parentElement && node.parentElement.closest('svg')) return;
    if (tag === 'IMG' && !isDrawableImg(node)) return;
    const type = tag === 'CANVAS' ? 'canvas' : tag === 'IMG' ? 'img' : 'svg';
    const size = intrinsicSize(node);
    const item = { type, el: node, order, w: size.w, h: size.h };

    if (laidOut) {
      // Hidden elements (`display: none`, collapsed cards) have no box, and
      // icons are small
      const r = node.getBoundingClientRect();
      if (r.width * r.height < MIN_VISUAL_AREA) return;
      // Content hidden with `visibility` (e.g. a closed data loader) keeps
      // its box; it is used only when nothing else is shown (see below)
      item.cssHidden = /^(hidden|collapse)$/.test(getComputedStyle(node).visibility);

      // Clip to the container (content scrolled out of view is left out)
      const left = Math.max(r.left, containerRect.left);
      const top = Math.max(r.top, containerRect.top);
      const right = Math.min(r.right, containerRect.right);
      const bottom = Math.min(r.bottom, containerRect.bottom);
      if (right <= left || bottom <= top) return;
      item.rect = r;
      item.clip = { left, top, right, bottom };
      item.z = stackLevel(node, el);
    } else if (!(item.w > 0 && item.h > 0)) {
      return;
    }
    candidates.push(item);
  });

  if (candidates.length === 0) return null;

  // Prefer content that is shown. While a page renders no frames (e.g. a
  // background browser tab), a pending screen switch can leave shown
  // content marked hidden; it is then used rather than returning nothing
  let hiddenNote = '';
  if (laidOut) {
    const shown = candidates.filter((c) => !c.cssHidden);
    if (shown.length) {
      candidates.splice(0, candidates.length, ...shown);
    } else {
      hiddenNote = 'The page hides this content with CSS (`visibility: hidden`), e.g. while a screen switch is pending in a background browser tab; it is captured anyway.';
    }
  }

  const names = { canvas: ['canvas', 'canvases'], img: ['image', 'images'], svg: ['SVG', 'SVGs'] };
  const kinds = (items) => {
    const counts = {};
    items.forEach((c) => { counts[c.type] = (counts[c.type] || 0) + 1; });
    return Object.keys(counts)
      .map((k) => `${counts[k]} ${names[k][counts[k] > 1 ? 1 : 0]}`).join(', ');
  };
  // A WebGL/WebGPU canvas that is not being redrawn cannot be read (see
  // canvasTo2D)
  const withBlankNote = (items, note) => {
    const n = items.filter((c) => c.snapshot && c.snapshot.gpuBlank).length;
    if (!n) return note;
    return [note, `${n === 1 ? 'A 3D (WebGL/WebGPU) canvas' : n + ' 3D (WebGL/WebGPU) canvases'} could not be read and ${n === 1 ? 'is' : 'are'} blank in this image: a 3D view keeps its pixels only while it redraws (e.g. right after a setting changes).`].filter(Boolean).join(' ');
  };

  try {
    // --- no layout: the largest element alone ------------------------------
    if (!laidOut) {
      const item = candidates.reduce((a, b) => (b.w * b.h > a.w * a.h ? b : a));
      await snapshotCanvases([item]);
      const note = candidates.length > 1
        ? `The element has no layout, so only the largest (${names[item.type][0]}) of its ${candidates.length} visual elements (${kinds(candidates)}) is shown.`
        : '';
      if (item.type === 'img') {
        const src = item.el.currentSrc || item.el.getAttribute('src') || '';
        if (src.startsWith('data:')) {
          const direct = splitDataURL(src);
          if (direct) return Object.assign(direct, { note });
        }
      }
      const canvas = document.createElement('canvas');
      const scale = Math.min(1, MAX_IMAGE_SIDE / Math.max(item.w, item.h));
      canvas.width = Math.round(item.w * scale);
      canvas.height = Math.round(item.h * scale);
      const ctx = canvas.getContext('2d');
      if (item.type === 'svg') {
        ctx.fillStyle = effectiveBackground(el);
        ctx.fillRect(0, 0, canvas.width, canvas.height);
      }
      if (!await drawVisual(ctx, item, 0, 0, canvas.width, canvas.height)) return null;
      const res = splitDataURL(canvas.toDataURL('image/png'));
      return res && Object.assign(res, { note: withBlankNote([item], note) });
    }

    // --- composite in paint order, cropped to the visual content -----------
    candidates.sort((a, b) => (a.z - b.z) || (a.order - b.order));
    await snapshotCanvases(candidates);
    const bounds = candidates.reduce((u, c) => ({
      left: Math.min(u.left, c.clip.left),
      top: Math.min(u.top, c.clip.top),
      right: Math.max(u.right, c.clip.right),
      bottom: Math.max(u.bottom, c.clip.bottom)
    }), { left: Infinity, top: Infinity, right: -Infinity, bottom: -Infinity });
    const bw = bounds.right - bounds.left;
    const bh = bounds.bottom - bounds.top;
    const scale = Math.min(1, MAX_IMAGE_SIDE / Math.max(bw, bh));

    const composite = document.createElement('canvas');
    composite.width = Math.max(1, Math.round(bw * scale));
    composite.height = Math.max(1, Math.round(bh * scale));
    const ctx = composite.getContext('2d');
    ctx.fillStyle = effectiveBackground(el);
    ctx.fillRect(0, 0, composite.width, composite.height);
    ctx.scale(scale, scale);

    let drawn = 0;
    for (const item of candidates) {
      const r = item.rect;
      try {
        if (await drawVisual(ctx, item, r.left - bounds.left, r.top - bounds.top, r.width, r.height)) {
          drawn++;
        }
      } catch (e) {
        // e.g. a tainted canvas: leave it out
      }
    }
    if (!drawn) return null;

    const res = splitDataURL(composite.toDataURL('image/png'));
    const note = candidates.length > 1
      ? `Composite of ${candidates.length} visual elements (${kinds(candidates)}) as laid out on screen.`
      : '';
    return res && Object.assign(res, {
      note: withBlankNote(candidates, [note, hiddenNote].filter(Boolean).join(' '))
    });
  } catch (e) {
    return null;
  }
}
