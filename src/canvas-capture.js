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
. * Whether `canvas` draws with WebGL or WebGPU. `getContext()` returns null
 * for a type other than the one a canvas already has, so a canvas with a
 * context is left as it is (the types are asked in `canvasTo2D`'s order).
 */
function isGPUCanvas(canvas) {
  for (const type of ['webgl2', 'webgl', 'webgpu']) {
    try {
      if (canvas.getContext(type)) return true;
    } catch (e) {
      // e.g. a canvas whose control was transferred to an OffscreenCanvas
    }
  }
  return false;
}

/** Whether the viewer answered a `viewerApp.captureOnce` request. */
function answered(request) {
  return !!request && typeof request.dataURI === 'string' &&
    request.dataURI.startsWith('data:image/');
}

/** The picture the viewer drew for `canvas` in an answered request, or null. */
function pictureFor(request, canvas) {
  if (!answered(request) || !Array.isArray(request.views)) return null;
  const view = request.views.find((v) => v && v.canvas === canvas);
  if (!view || typeof view.dataURI !== 'string' ||
      !view.dataURI.startsWith('data:image/')) return null;
  return view.dataURI;
}

/**
 * Copy the canvases in `items` to images or 2D canvases (`item.snapshot`).
 *
 * The threeBrain viewer draws only when something changes, and a copy of a
 * WebGPU canvas taken outside the frame that drew it can be blank (Chromium)
 * or out of date. So each viewer holding a WebGL/WebGPU canvas in `items`
 * gets one `viewerApp.captureOnce` event, dispatched on its wrapper
 * (`.threejs-brain-canvas`), with an empty object as `detail`: the viewer
 * draws a frame, adds `{ canvas, dataURI }` (a PNG data URL) to
 * `detail.views` for each view it drew, and sets `detail.dataURI`. Canvases
 * with no picture once every request is answered, or after `maxFrames`
 * animation frames (a hidden viewer, an older build), and all other canvases
 * (the viewer's 2D overlay, other widgets) are copied directly in that frame.
 * Pages that run no animation frames (a background tab, a hidden module) are
 * copied after `timeoutMs`.
 * Resolves true when the copies ran in an animation frame, false after the
 * timeout.
 */
async function snapshotCanvases(items, timeoutMs = 500, maxFrames = 3) {
  const canvases = items.filter((item) => item.type === 'canvas');
  // One request per viewer, however many of its canvases are copied
  const requests = new Map();
  canvases.forEach((item) => {
    item.request = null;
    const wrapper = item.el.closest('.threejs-brain-canvas');
    if (!wrapper || !isGPUCanvas(item.el)) return;
    if (!requests.has(wrapper)) {
      const request = {};
      requests.set(wrapper, request);
      wrapper.dispatchEvent(new CustomEvent('viewerApp.captureOnce', { detail: request }));
    }
    item.request = requests.get(wrapper);
  });
  const copyDirectly = (item) => {
    try {
      item.snapshot = canvasTo2D(item.el);
    } catch (e) {
      // e.g. a tainted canvas: leave it out
      item.snapshot = null;
    }
  };

  const inFrame = await new Promise((resolve) => {
    let done = false;
    let frames = 0;
    let frame = null;
    let timer = null;
    const finish = (ran) => {
      if (done) return;
      done = true;
      cancelAnimationFrame(frame);
      clearTimeout(timer);
      canvases.forEach((item) => {
        if (!pictureFor(item.request, item.el)) copyDirectly(item);
      });
      resolve(ran);
    };
    const onFrame = () => {
      frames++;
      const waiting = Array.from(requests.values()).some((request) => !answered(request));
      if (!waiting || frames >= maxFrames) {
        finish(true);
        return;
      }
      frame = requestAnimationFrame(onFrame);
    };
    frame = requestAnimationFrame(onFrame);
    timer = setTimeout(() => finish(false), timeoutMs);
  });

  // The pictures the viewers drew
  await Promise.all(canvases.filter((item) => pictureFor(item.request, item.el)).map(async (item) => {
    const img = new Image();
    img.src = pictureFor(item.request, item.el);
    try {
      await img.decode();
      item.snapshot = img;
    } catch (e) {
      copyDirectly(item);
    }
  }));
  return inFrame;
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
  // A WebGL/WebGPU canvas that drew no frame for the copy cannot be read
  // (see canvasTo2D), and canvases on a page that runs no animation frames
  // are copied without one (see snapshotCanvases)
  const withBlankNote = (items, note, inFrame) => {
    const notes = [note];
    if (!inFrame && items.some((c) => c.type === 'canvas')) {
      notes.push('The page drew no animation frame for this image (e.g. it is in a background browser tab or a hidden module), so its canvases may be blank or out of date.');
    }
    const n = items.filter((c) => c.snapshot && c.snapshot.gpuBlank).length;
    if (n) {
      notes.push(`${n === 1 ? 'A 3D (WebGL/WebGPU) canvas' : n + ' 3D (WebGL/WebGPU) canvases'} could not be read and ${n === 1 ? 'is' : 'are'} blank in this image: such a canvas can be read only during a frame it draws, and ${n === 1 ? 'it' : 'they'} drew none for this image.`);
    }
    return notes.filter(Boolean).join(' ');
  };

  try {
    // --- no layout: the largest element alone ------------------------------
    if (!laidOut) {
      const item = candidates.reduce((a, b) => (b.w * b.h > a.w * a.h ? b : a));
      const inFrame = await snapshotCanvases([item]);
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
      return res && Object.assign(res, { note: withBlankNote([item], note, inFrame) });
    }

    // --- composite in paint order, cropped to the visual content -----------
    candidates.sort((a, b) => (a.z - b.z) || (a.order - b.order));
    const inFrame = await snapshotCanvases(candidates);
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
      note: withBlankNote(candidates, [note, hiddenNote].filter(Boolean).join(' '), inFrame)
    });
  } catch (e) {
    return null;
  }
}
