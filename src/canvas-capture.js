/**
 * Canvas capture helpers for query_ui / MCP visual content extraction.
 *
 * Handles multiple overlapping canvases (e.g. WebGL + 2D overlay) and
 * <img> elements with data-URI src.
 */

/**
 * Copy a canvas (2D or WebGL) onto a temporary 2D canvas.
 * For WebGL canvases without preserveDrawingBuffer, drawImage may return
 * blank pixels.  In that case we fall back to readPixels.
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
  if (!gl) return tmp;

  const pixels = new Uint8Array(w * h * 4);
  gl.readPixels(0, 0, w, h, gl.RGBA, gl.UNSIGNED_BYTE, pixels);

  // Check if readPixels actually got data
  let hasData = false;
  for (let i = 3; i < pixels.length; i += 200) {
    if (pixels[i] !== 0) { hasData = true; break; }
  }
  if (!hasData) return tmp;

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

export function captureVisualContent(el) {
  return new Promise((resolve) => {
    resolve(_captureVisualContentSync(el));
  });
}

function _captureVisualContentSync(el) {
  // --- gather candidates -----------------------------------------------------
  const candidates = []; // { type: 'canvas'|'img', el, w, h }

  const addCanvas = (c) => {
    const w = c.width || c.offsetWidth || 0;
    const h = c.height || c.offsetHeight || 0;
    if (w > 0 && h > 0) {
      candidates.push({ type: 'canvas', el: c, w, h });
    }
  };

  const addImg = (img) => {
    const src = img.getAttribute('src') || '';
    if (!src.startsWith('data:')) return;
    const w = img.naturalWidth || img.width || 0;
    const h = img.naturalHeight || img.height || 0;
    if (w > 0 && h > 0) {
      candidates.push({ type: 'img', el: img, w, h, src });
    }
  };

  // If the element itself is a canvas, it is the only candidate
  if (el.tagName === 'CANVAS') {
    addCanvas(el);
  } else {
    el.querySelectorAll('canvas').forEach(addCanvas);
    el.querySelectorAll('img[src^="data:"]').forEach(addImg);
  }

  if (candidates.length === 0) return null;

  // --- composite all candidates with relative positioning -------------------
  const containerRect = el.getBoundingClientRect();

  // Use the container's dimensions for the composite canvas
  const w = Math.round(containerRect.width) || Math.max(...candidates.map(c => c.w));
  const h = Math.round(containerRect.height) || Math.max(...candidates.map(c => c.h));
  const composite = document.createElement('canvas');
  composite.width = w;
  composite.height = h;
  const ctx = composite.getContext('2d');

  for (let i = candidates.length - 1; i >= 0; i--) {
    const item = candidates[i];
    const itemRect = item.el.getBoundingClientRect();
    const dx = itemRect.left - containerRect.left;
    const dy = itemRect.top - containerRect.top;
    if (item.type === 'canvas') {
      ctx.drawImage(canvasTo2D(item.el), dx, dy, itemRect.width, itemRect.height);
    } else {
      ctx.drawImage(item.el, dx, dy, itemRect.width, itemRect.height);
    }
  }

  // --- export to base-64 PNG -------------------------------------------------
  const dataUrl = composite.toDataURL('image/png');
  const parts = dataUrl.split(',');
  const mime = (parts[0] || '').replace(/^data:/, '').replace(/;base64$/, '') || 'image/png';
  const data = parts[1] || '';
  if (data) return { image_data: data, image_type: mime };
  return null;
}
