/**
 * Shidashi - Bootstrap 5 / bslib dashboard toolkit
 *
 * jQuery is used for Shiny output bindings (find() must return jQuery objects)
 * and for Shiny custom events (shiny:connected is a jQuery event).
 * jQuery is provided by Shiny at runtime and treated as external by esbuild.
 */

import $ from 'jquery';
import { IFrameManager } from './iframe-manager.js';
import { Sidebar } from './sidebar.js';
import ClipboardJS from 'clipboard';

// ============================================================================
// Output Bindings (registered once Shiny is available)
// ============================================================================

function registerOutputBindings() {
  if (!window.Shiny) return;
  const Shiny = window.Shiny;

  // --- Progress Output Binding ---
  const progressOutputBinding = new Shiny.OutputBinding();
  progressOutputBinding.name = 'shidashi.progressOutputBinding';

  $.extend(progressOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shidashi-progress-output');
    },
    renderValue: function(el, value) {
      let v = parseInt(value.value);
      if (isNaN(v)) return;
      if (v < 0) v = 0;
      if (v > 100) v = 100;
      $(el).find('.progress-bar').css('width', v + '%');
      if (typeof value.description === 'string') {
        $(el).find('.progress-description.progress-message').text(value.description);
      }
    },
    renderError: function(el, err) {
      if (err.message === 'argument is of length zero') {
        $(el).removeClass('shidashi-progress-error');
        $(el).find('.progress-bar').css('width', '0%');
      } else {
        $(el).addClass('shidashi-progress-error')
          .find('.progress-description.progress-error')
          .text(err.message);
      }
    },
    clearError: function(el) {
      $(el).removeClass('shidashi-progress-error');
    }
  });

  Shiny.outputBindings.register(progressOutputBinding, 'shidashi.progressOutputBinding');

  // --- Clipboard Output Binding ---
  const clipboardOutputBinding = new Shiny.OutputBinding();
  clipboardOutputBinding.name = 'shidashi.clipboardOutputBinding';

  $.extend(clipboardOutputBinding, {
    find: function(scope) {
      return $(scope).find('.shidashi-clipboard-output');
    },
    renderValue: function(el, value) {
      let el_ = $(el);
      if (!el_.hasClass('clipboard-btn')) {
        el_ = $(el).find('.clipboard-btn');
      }
      el_.attr('data-clipboard-text', value);
    },
    renderError: function(el, err) {
      let el_ = $(el);
      if (!el_.hasClass('clipboard-btn')) {
        el_ = $(el).find('.clipboard-btn');
      }
      el_.attr('data-clipboard-text', 'Error: ' + err.message);
    }
  });

  Shiny.outputBindings.register(clipboardOutputBinding, 'shidashi.clipboardOutputBinding');

  // Clipboard click handler (delegation)
  new ClipboardJS('.clipboard-btn').on('success', (e) => {
    window.shidashi.createNotification({
      title: 'Copied to clipboard',
      delay: 1000,
      autohide: true,
      icon: 'fa fas fa-copy',
      class: 'bg-success'
    });
    e.clearSelection();
  });
}

// ============================================================================
// Shidashi Main Class
// ============================================================================

class ShidashiApp {
  constructor() {
    this._shiny = null;
    this._dummy = document.createElement('div');
    this._dummy2 = document.createElement('div');
    this._localStorage = window.localStorage;
    this._sessionStorage = window.sessionStorage;
    this._keyPrefix = 'shidashi-session-';
    this._keyNotification = 'shidashi-session';
    this._keyTheme = 'shidashi-theme';
    this._listeners = {};
    this._storageDuration = 1000 * 60 * 60 * 24; // 1 day (matches original)
    this.sessionData = {};
    this._initialized = false;
    this._shiny_registered = false;
    this.shiny_connected = false;
    this._shiny_callstacks = [];
    this.sidebar = null;
    this.iframeManager = null;
    this._toastCounter = 0;

    // RAVE-specific state
    this._moduleId = undefined;
    this._raveId = undefined;
    this._active_module = undefined;
    this._bodyClasses = [];
    this.variableBodyClasses = ['scroller-not-top', 'navbar-hidden'];
  }

  // ---------- Shiny helper ----------

  ensureShiny(then) {
    if (!this._shiny) {
      this._shiny = window.Shiny;
    }
    if (typeof then === 'function') {
      this._shiny_callstacks.push(then);
    }
    if (document.readyState && document.readyState === 'complete' &&
        this._shiny && this.shiny_connected) {
      while (this._shiny_callstacks.length > 0) {
        const f = this._shiny_callstacks.shift();
        try {
          f(this._shiny);
        } catch (e) {
          console.warn(e);
        }
      }
    }
  }

  bindAll(el, ensure = true) {
    const b = (shiny) => {
      shiny.bindAll(el);
      // Also report active tabs in any tabsets within el
      const tabLists = (el instanceof HTMLElement ? el : document).querySelectorAll('.card-tabs [role="tablist"]');
      for (let ii = 0; ii < tabLists.length; ii++) {
        const pa = tabLists[ii];
        if (pa && pa.id) {
          const activeTab = pa.querySelector('li.nav-item > .nav-link.active');
          if (activeTab) {
            shiny.setInputValue(pa.id, activeTab.textContent);
          }
        }
      }
    };
    if (ensure || this._shiny) {
      this.ensureShiny(b);
    }
  }

  unbindAll(el, ensure = true) {
    const ub = (shiny) => {
      shiny.unbindAll(el);
    };
    if (ensure || this._shiny) {
      this.ensureShiny(ub);
    }
  }

  // ---------- localStorage session sync ----------

  fromLocalStorage(key, defaultIfNotFound, ignoreDuration = false) {
    try {
      const item = JSON.parse(this._localStorage.getItem(key));
      item.last_saved = new Date(item.last_saved);
      item._key = key;
      if (!ignoreDuration) {
        const now = new Date();
        if (now - item.last_saved > this._storageDuration) {
          console.debug('Removing expired key: ' + key);
          this._localStorage.removeItem(key);
        } else {
          return item;
        }
      } else {
        return item;
      }
    } catch (e) {
      console.debug('Removing corrupted key: ' + key);
      this._localStorage.removeItem(key);
    }
    if (defaultIfNotFound === true) {
      return {
        inputs: {},
        last_saved: new Date(),
        last_edit: this._private_id,
        inputs_changed: [],
        _key: key
      };
    }
    return defaultIfNotFound;
  }

  async cleanLocalStorage(maxEntries = 1000) {
    const items = [];
    for (let key in this._localStorage) {
      if (key.startsWith(this._keyPrefix)) {
        const item = this.fromLocalStorage(key);
        if (maxEntries && item) {
          items.push(item);
        }
      }
    }
    if (items.length && items.length > maxEntries) {
      items.sort((v1, v2) => v1.last_saved > v2.last_saved ? 1 : -1);
      items.splice(items.length - maxEntries);
      items.forEach((item) => {
        this._localStorage.removeItem(item._key);
      });
    }
  }

  _setSharedId(shared_id) {
    if (typeof this._shared_id !== 'string' && typeof shared_id === 'string') {
      this._shared_id = shared_id;
      this._storage_key = this._keyPrefix + this._shared_id;
    }
    return this._storage_key;
  }

  _setPrivateId(private_id) {
    if (typeof this._private_id !== 'string') {
      if (typeof private_id === 'string') {
        this._private_id = private_id;
      } else {
        this._private_id = Math.random().toString(16).substr(2, 8);
      }
    }
    return this._private_id;
  }

  broadcastSessionData(shared_id, private_id) {
    const storage_key = this._setSharedId(shared_id);
    if (!storage_key) return;
    const private_id_ = this._setPrivateId(private_id);

    const keys_changed = Object.keys(this.sessionData);
    if (!keys_changed.length) return;

    const now = new Date();
    const stored = this.fromLocalStorage(storage_key, true, true);
    stored.last_saved = now;
    stored.last_edit = private_id_;
    stored.inputs_changed = keys_changed;
    for (let k in this.sessionData) {
      stored.inputs[k] = this.sessionData[k];
    }
    this._localStorage.setItem(storage_key, JSON.stringify(stored));
    this._localStorage.setItem(this._keyNotification, JSON.stringify({
      storage_key: storage_key,
      private_id: private_id_,
      last_saved: now
    }));
  }

  // ---------- Event system ----------

  broadcastEvent(type, message = {}) {
    const event = new CustomEvent('shidashi-event-' + type, { detail: message });
    this._dummy.dispatchEvent(event);
    this.ensureShiny(() => {
      if (typeof this._shiny.onInputChange !== 'function') return;
      this._shiny.onInputChange('@shidashi_event@', {
        type: type,
        message: message,
        shared_id: this._shared_id,
        private_id: this._private_id
      });
    });
    // Propagate to managed iframes
    if (this.iframeManager) {
      this.iframeManager.notifyIframes(type, message);
    }
  }

  registerListener(type, callback, replace = true) {
    const event_str = 'shidashi-event-' + type;
    if (replace) {
      const old_function = this._listeners[type];
      if (typeof old_function === 'function') {
        this._dummy.removeEventListener(event_str, old_function);
      }
    }
    if (typeof callback === 'function') {
      const cb_ = (evt) => callback(evt.detail);
      this._dummy.addEventListener(event_str, cb_);
      this._listeners[type] = cb_;
    }
  }

  // ---------- Theme ----------

  _col2Hex(color, fallback) {
    let col = color.trim();
    if (col.length < 4) return fallback || '#000000';
    if (col[0] === '#') {
      if (col.length === 7) return col;
      col = '#' + col[1] + col[1] + col[2] + col[2] + col[3] + col[3];
      return col;
    }
    // Handle rgba with alpha=0 (transparent) — return fallback
    const rgbaMatch = col.match(/rgba\((\d+),\s*(\d+),\s*(\d+),\s*([\d.]+)\)/);
    if (rgbaMatch && parseFloat(rgbaMatch[4]) === 0) {
      return fallback || '#ffffff';
    }
    let parts = col.match(/rgb[a]?\((\d+),\s*(\d+),\s*(\d+)[\),]/);
    if (!parts) return fallback || '#000000';
    delete parts[0];
    for (let i = 1; i <= 3; ++i) {
      parts[i] = parseInt(parts[i]).toString(16);
      if (parts[i].length === 1) parts[i] = '0' + parts[i];
    }
    col = '#' + parts.slice(1, 4).join('');
    return col;
  }

  _reportTheme(mode) {
    if (typeof mode !== 'string') {
      mode = this.isDarkMode() ? 'dark' : 'light';
    }
    const darkFallbackBg = '#343a40';
    const lightFallbackBg = '#ffffff';
    const darkFallbackFg = '#e9ecef';
    const lightFallbackFg = '#343a40';
    const fallbackBg = mode === 'dark' ? darkFallbackBg : lightFallbackBg;
    const fallbackFg = mode === 'dark' ? darkFallbackFg : lightFallbackFg;

    // Defer to next frame so computed styles reflect the class change
    requestAnimationFrame(() => {
      const body = document.body;
      const cardEl = document.querySelector('.card, .info-box');
      let bgcolor;
      if (cardEl) {
        bgcolor = this._col2Hex(getComputedStyle(cardEl).backgroundColor, fallbackBg);
      } else {
        bgcolor = this._col2Hex(getComputedStyle(body).backgroundColor, fallbackBg);
      }
      this.broadcastEvent('theme.changed', {
        mode: mode,
        background: bgcolor,
        foreground: this._col2Hex(getComputedStyle(body).color, fallbackFg)
      });
    });
  }

  isDarkMode() {
    return document.body.classList.contains('dark-mode');
  }

  asLightMode() {
    document.body.classList.remove('dark-mode');
    // Sidebar stays dark by default in light mode (rave-pipelines convention)
    // Only switch to sidebar-light if the sidebar has the explicit opt-in class
    const aside = document.querySelector('.shidashi-sidebar');
    if (aside && aside.classList.contains('shidashi-sidebar--follow-theme')) {
      aside.classList.remove('sidebar-dark');
      aside.classList.add('sidebar-light');
    }
    // Header navbar theme
    const header = document.querySelector('.shidashi-header');
    if (header) {
      header.setAttribute('data-bs-theme', 'light');
    }
    this._sessionStorage.setItem(this._keyTheme, 'light');
    // Propagate to iframes
    if (this.iframeManager) {
      this.iframeManager.propagateThemeToAll();
    }
    this._reportTheme('light');
  }

  asDarkMode() {
    document.body.classList.add('dark-mode');
    const aside = document.querySelector('.shidashi-sidebar');
    if (aside) {
      aside.classList.remove('sidebar-light');
      aside.classList.add('sidebar-dark');
    }
    // Header navbar theme
    const header = document.querySelector('.shidashi-header');
    if (header) {
      header.setAttribute('data-bs-theme', 'dark');
    }
    this._sessionStorage.setItem(this._keyTheme, 'dark');
    if (this.iframeManager) {
      this.iframeManager.propagateThemeToAll();
    }
    this._reportTheme('dark');
  }

  // ---------- UI actions ----------

  click(selector) {
    if (!selector || selector === '') return;
    const el = document.querySelector(selector);
    if (el) el.click();
  }

  triggerResize(timeout) {
    if (timeout) {
      setTimeout(() => this.triggerResize(), timeout);
    } else {
      window.dispatchEvent(new Event('resize'));
      this.ensureShiny(() => {
        this._shiny.unbindAll(this._dummy2);
      });
    }
  }

  // ---------- Tabset (card-tabset) ----------

  tabsetAdd(inputId, title, body, active = true) {
    const el = document.getElementById(inputId);
    const elbody = document.getElementById(inputId + 'Content');
    if (!el) return 'Cannot find tabset with given settings.';
    if (!elbody) return 'Cannot find tabset with given settings.';

    // Check for duplicate title
    const existingHeaders = el.querySelectorAll(':scope > .nav-item.nav-tab-header');
    for (const item of existingHeaders) {
      const link = item.querySelector('.nav-link');
      if (link && link.textContent === title) {
        return "A tab with title '" + title + "' already exists.";
      }
    }

    const tabId = Math.random().toString(16).substr(2, 8);

    // Create header
    const headerItem = document.createElement('li');
    headerItem.className = 'nav-item nav-tab-header';
    const headerA = document.createElement('a');
    headerA.className = 'nav-link';
    headerA.setAttribute('href', `#${inputId}-${tabId}`);
    headerA.setAttribute('id', `${inputId}-${tabId}-tab`);
    headerA.setAttribute('data-bs-toggle', 'tab');
    headerA.setAttribute('role', 'tab');
    headerA.setAttribute('aria-controls', `${inputId}-${tabId}`);
    headerA.setAttribute('aria-selected', 'false');
    headerA.textContent = title;
    headerItem.appendChild(headerA);

    // Insert after last header
    if (existingHeaders.length > 0) {
      existingHeaders[existingHeaders.length - 1].after(headerItem);
    } else {
      el.appendChild(headerItem);
    }

    // Create body pane
    const bodyEl = document.createElement('div');
    bodyEl.className = 'tab-pane fade';
    bodyEl.setAttribute('id', `${inputId}-${tabId}`);
    bodyEl.setAttribute('role', 'tabpanel');
    bodyEl.setAttribute('tab-index', tabId);
    bodyEl.setAttribute('aria-labelledby', `${inputId}-${tabId}-tab`);
    bodyEl.innerHTML = body;
    elbody.appendChild(bodyEl);

    this.bindAll(elbody);

    if (active) {
      return this.tabsetActivate(inputId, title);
    }
    return true;
  }

  tabsetRemove(inputId, title) {
    const el = document.getElementById(inputId);
    const elbody = document.getElementById(inputId + 'Content');
    if (!el) return 'Cannot find tabset with given settings.';
    if (!elbody) return 'Cannot find tabset with given settings.';

    const existingItems = el.querySelectorAll(':scope > .nav-item.nav-tab-header');
    if (!existingItems.length) {
      return "Tab with title '" + title + "' cannot be found.";
    }

    let found = false;
    let activate = false;
    let removeIdx = -1;

    existingItems.forEach((item, i) => {
      const link = item.querySelector('.nav-link');
      if (link && link.textContent === title) {
        found = true;
        removeIdx = i;
        const tabid = link.getAttribute('aria-controls');
        const tab = document.getElementById(tabid);
        const isActive = link.getAttribute('aria-selected');
        this.unbindAll(tab);
        item.remove();
        if (tab) tab.remove();
        if (isActive === 'true') {
          activate = true;
        }
      }
    });

    if (!found) {
      return "A tab with title '" + title + "' cannot be found.";
    }

    if (activate && existingItems.length > 1) {
      let activeTab;
      if (removeIdx - 1 >= 0) {
        activeTab = existingItems[removeIdx - 1];
      } else {
        activeTab = existingItems[removeIdx + 1];
      }
      if (activeTab) {
        const link = activeTab.querySelector('a.nav-link');
        if (link) link.click();
      }
    }
    return true;
  }

  tabsetActivate(inputId, title) {
    const el = document.getElementById(inputId);
    const elbody = document.getElementById(inputId + 'Content');
    if (!el) return 'Cannot find tabset with given settings.';
    if (!elbody) return 'Cannot find tabset with given settings.';

    const existingItems = el.querySelectorAll(':scope > .nav-item.nav-tab-header');
    if (!existingItems.length) {
      return "Tab with title '" + title + "' cannot be found.";
    }

    let activated = false;
    existingItems.forEach((item) => {
      const link = item.querySelector('.nav-link');
      if (!link) return;
      const paneId = link.getAttribute('aria-controls');
      const pane = paneId ? document.getElementById(paneId) : null;
      if (link.textContent === title) {
        link.click();
        activated = true;
      } else {
        link.classList.remove('active');
        link.setAttribute('aria-selected', 'false');
        if (pane) {
          pane.classList.remove('show', 'active');
        }
      }
    });

    if (!activated) {
      return "Tab with title '" + title + "' cannot be found.";
    }
    return true;
  }

  // ---------- Card / card2 / flip-box ----------

  card(inputId, method) {
    // ravedash may call with a single object: {selector, method}
    if (typeof inputId === 'object' && inputId !== null) {
      const opts = inputId;
      if (opts.selector) {
        document.querySelectorAll(opts.selector).forEach((el) => {
          const card = el.closest('.card') || el;
          this._cardOperate(card, opts.method);
        });
        return;
      }
    }
    const el = document.getElementById(inputId);
    if (!el) return;
    const card = el.closest('.card') || el;
    this._cardOperate(card, method);
  }

  _cardOperate(card, method) {
    if (!card) return;
    switch (method) {
      case 'collapse':
        // CSS on .card.shidashi-collapsed handles the soft-hide:
        // height:0 + overflow:hidden keeps the element in the DOM so
        // Shiny outputs retain defined width and continue to update.
        card.classList.add('shidashi-collapsed');
        this._updateCardIcon(card, true);
        break;

      case 'minimize':
        // Reverse of maximize: restore the card from fullscreen
        card.classList.remove('shidashi-maximized');
        document.body.classList.remove('shidashi-card-maximized');
        this._updateMaximizeIcon(card);
        this.triggerResize(50);
        break;

      case 'expand':
        card.classList.remove('shidashi-collapsed');
        if (card.classList.contains('start-collapsed')) {
          this.unbindAll(card);
          card.classList.remove('start-collapsed');
          this.bindAll(card);
        }
        this._updateCardIcon(card, false);
        this.triggerResize(50);
        break;

      case 'maximize':
        card.classList.add('shidashi-maximized');
        document.body.classList.add('shidashi-card-maximized');
        this._updateMaximizeIcon(card);
        this.triggerResize(50);
        break;

      case 'toggleMaximize':
        if (method === 'toggleMaximize' && card.classList.contains('shidashi-maximized')) {
          card.classList.remove('shidashi-maximized');
          document.body.classList.remove('shidashi-card-maximized');
        } else {
          card.classList.add('shidashi-maximized');
          document.body.classList.add('shidashi-card-maximized');
        }
        this._updateMaximizeIcon(card);
        this.triggerResize(50);
        break;

      case 'restore':
        card.classList.remove('shidashi-maximized');
        document.body.classList.remove('shidashi-card-maximized');
        this._updateMaximizeIcon(card);
        this.triggerResize(50);
        break;

      case 'toggle':
        if (card.classList.contains('shidashi-collapsed') || card.classList.contains('start-collapsed')) {
          this._cardOperate(card, 'expand');
        } else {
          this._cardOperate(card, 'collapse');
        }
        break;

      case 'remove':
        card.remove();
        break;

      default:
        break;
    }
  }

  _updateCardIcon(card, collapsed) {
    const icon = card.querySelector('[data-card-widget="collapse"] i, [data-card-widget="collapse"] .fas');
    if (icon) {
      if (collapsed) {
        icon.classList.remove('fa-minus');
        icon.classList.add('fa-plus');
      } else {
        icon.classList.remove('fa-plus');
        icon.classList.add('fa-minus');
      }
    }
  }

  _updateMaximizeIcon(card) {
    const btn = card.querySelector('[data-card-widget="maximize"]');
    if (!btn) return;
    const icon = btn.querySelector('i, .fas');
    if (icon) {
      if (card.classList.contains('shidashi-maximized')) {
        icon.classList.remove('fa-expand');
        icon.classList.add('fa-compress');
      } else {
        icon.classList.remove('fa-compress');
        icon.classList.add('fa-expand');
      }
    }
  }

  toggleCard2(selector) {
    const el = document.querySelector(selector);
    if (!el) return;
    // Match original AdminLTE3 behavior: click the button to trigger DirectChat toggle
    el.click();
  }

  flipBox(inputId) {
    const el = document.getElementById(inputId);
    if (el && el.classList.contains('flip-box')) {
      el.classList.toggle('active');
    }
  }

  // ---------- Notification (BS5 Toast) ----------

  createNotification(options) {
    const container = this._getToastContainer();
    const id = 'shidashi-toast-' + (++this._toastCounter);

    const toastEl = document.createElement('div');
    toastEl.id = id;
    toastEl.className = 'toast';
    toastEl.setAttribute('role', 'alert');
    toastEl.setAttribute('aria-live', 'assertive');
    toastEl.setAttribute('aria-atomic', 'true');

    if (options.class) {
      toastEl.classList.add(...options.class.split(/\s+/));
    }
    if (options.autohide === false) {
      toastEl.setAttribute('data-bs-autohide', 'false');
    } else {
      toastEl.setAttribute('data-bs-autohide', 'true');
      toastEl.setAttribute('data-bs-delay', String(options.delay || 5000));
    }

    // Build toast content
    let headerHtml = '';
    if (options.icon) {
      headerHtml += `<i class="${options.icon} me-2"></i>`;
    }
    if (options.image) {
      headerHtml += `<img src="${this._escapeAttr(options.image)}" class="rounded me-2" alt="" style="width:20px;height:20px;">`;
    }
    headerHtml += `<strong class="me-auto">${this._escapeHtml(options.title || '')}</strong>`;
    if (options.subtitle) {
      headerHtml += `<small>${this._escapeHtml(options.subtitle)}</small>`;
    }

    toastEl.innerHTML = `
      <div class="toast-header">
        ${headerHtml}
        <button type="button" class="btn-close" data-bs-dismiss="toast" aria-label="Close"></button>
      </div>
      ${options.body ? `<div class="toast-body">${options.body}</div>` : ''}
    `;

    container.appendChild(toastEl);

    // Bind Shiny outputs inside toast
    this.ensureShiny(() => {
      this._shiny.bindAll(toastEl);
    });

    // Show via BS5 Toast API
    if (window.bootstrap?.Toast) {
      const toast = new bootstrap.Toast(toastEl);
      toast.show();

      // Remove from DOM after hidden
      toastEl.addEventListener('hidden.bs.toast', () => {
        this.ensureShiny(() => {
          this._shiny.unbindAll(toastEl);
        });
        toastEl.remove();
      });
    }
  }

  clearNotification(selector) {
    const els = document.querySelectorAll(selector || '.toast');
    els.forEach(el => {
      if (window.bootstrap?.Toast) {
        const instance = bootstrap.Toast.getInstance(el);
        if (instance) {
          instance.hide();
          return;
        }
      }
      el.remove();
    });
  }

  _getToastContainer() {
    let container = document.getElementById('shidashi-toast-container');
    if (!container) {
      container = document.createElement('div');
      container.id = 'shidashi-toast-container';
      container.className = 'toast-container position-fixed top-0 end-0 p-3';
      container.style.zIndex = '1100';
      document.body.appendChild(container);
    }
    return container;
  }

  // ---------- Progress ----------

  setProgress(inputId, value, max = 100, description = null) {
    if (typeof value !== 'number' || isNaN(value)) return;
    const el = document.getElementById(inputId);
    if (!el) return;

    let v = parseInt(value / max * 100);
    v = Math.max(0, Math.min(100, v));
    const bar = el.querySelector('.progress-bar');
    if (bar) bar.style.width = v + '%';
    if (typeof description === 'string') {
      const desc = el.querySelector('.progress-description.progress-message');
      if (desc) desc.textContent = description;
    }
  }

  // ---------- RAVE helpers ----------

  shinySetInput(inputId, value, add_timestamp = true, children = false) {
    this.ensureShiny((shiny) => {
      if (add_timestamp) {
        value.timestamp = new Date();
      }
      value._active_module = this._active_module;
      value.parent_frame = document.body.classList.contains('parent-frame');
      shiny.setInputValue(inputId, value);

      console.debug(`[shidashi] shiny input [${inputId}] <- ${JSON.stringify(value)}`);

      if (children && this.iframeManager) {
        // Clone the base payload (without parent-specific fields) so each
        // child can enrich it with its own _active_module & parent_frame.
        const baseValue = Object.assign({}, value);
        delete baseValue._active_module;
        delete baseValue.parent_frame;
        delete baseValue.timestamp;

        this.iframeManager._tabs.forEach(entry => {
          try {
            const win = entry.iframe.contentWindow;
            if (win?.shidashi) {
              // Use the child's shinySetInput so _active_module and
              // parent_frame are set from the child's own context.
              win.shidashi.shinySetInput(
                inputId,
                Object.assign({}, baseValue),
                add_timestamp,
                false
              );
            } else if (win?.Shiny?.setInputValue) {
              // Fallback: child doesn't have shidashi, set directly
              const childValue = Object.assign({}, baseValue);
              if (add_timestamp) { childValue.timestamp = new Date(); }
              childValue.parent_frame = false;
              win.Shiny.setInputValue(inputId, childValue);
            }
          } catch (e) { /* cross-origin safety */ }
        });
      }
    });
  }

  openURL(url, target = '_blank') {
    const link = document.createElement('a');
    link.setAttribute('target', target);
    link.setAttribute('href', url);
    link.click();
    this.createNotification({
      title: 'Opening the link',
      autohide: true,
      delay: 10000,
      icon: 'fas fa-link',
      subtitle: 'Link created',
      body: `A new window with the given link has been opened. If you haven't seen it, please <a href="${url}" target="${target}">click here</a>.`
    });
  }

  launchStandaloneViewer(outputId) {
    const url = `?output_id=${outputId}&rave_id=${this._raveId}&module=standalone_viewer`;
    this.openURL(url);
  }

  addClass(selector, cls) {
    document.querySelectorAll(selector).forEach(el => {
      el.classList.add(...cls.split(/\s+/).filter(Boolean));
    });
    if (selector.startsWith('body')) {
      this._bodyClasses = document.body.classList;
    }
  }

  removeClass(selector, cls) {
    document.querySelectorAll(selector).forEach(el => {
      el.classList.remove(...cls.split(/\s+/).filter(Boolean));
    });
    if (selector.startsWith('body')) {
      this._bodyClasses = document.body.classList;
    }
  }

  setInnerHtml(selector, content) {
    const els = document.querySelectorAll(selector);
    els.forEach(el => {
      this.unbindAll(el, false);
      el.innerHTML = content;
      this.bindAll(el, false);
    });
  }

  notifyParent(method, args) {
    if (window.parent && window.parent !== window) {
      if (window.parent.shidashi && typeof window.parent.shidashi[method] === 'function') {
        window.parent.shidashi[method](...args);
      }
    }
  }

  notifyIframes(method, args) {
    if (this.iframeManager) {
      this.iframeManager._tabs.forEach(entry => {
        try {
          if (entry.iframe.contentWindow?.shidashi) {
            entry.iframe.contentWindow.shidashi[method](...args);
          }
        } catch (e) {}
      });
    }
  }

  resumeStatus(parentShidashi) {
    if (!parentShidashi) return;
    if (parentShidashi._active_module !== this._moduleId) return;
    this.variableBodyClasses.forEach(cls => {
      if (this._bodyClasses?.contains?.(cls)) {
        parentShidashi.addClass('body', cls);
      } else {
        parentShidashi.removeClass('body', cls);
      }
    });
  }

  // ---------- Scroll ----------

  scrollTop(duration = 200) {
    window.scrollTo({ top: 0, behavior: duration > 0 ? 'smooth' : 'instant' });
  }

  // ---------- Utils ----------

  async matchSelector(el, selector, next, strict = false) {
    if (!el) return;
    const els = document.querySelectorAll(selector);
    if (!els.length) return;

    for (const item of els) {
      if (item === el || (!strict && item.contains(el))) {
        if (typeof next === 'function') {
          return next(item);
        }
        return true;
      }
    }
  }

  shinyHandler(action, callback) {
    if (!this._shiny) {
      if (window.Shiny) {
        this._shiny = window.Shiny;
      } else {
        console.error('Cannot find window.Shiny object. Is R-shiny running?');
        return false;
      }
    }
    this._shiny.addCustomMessageHandler('shidashi.' + action, callback);
  }

  _escapeHtml(str) {
    const div = document.createElement('div');
    div.textContent = str || '';
    return div.innerHTML;
  }

  _escapeAttr(str) {
    return (str || '').replace(/"/g, '&quot;').replace(/'/g, '&#39;');
  }

  // ---------- Card tool click delegation ----------

  _bindCardTools() {
    document.addEventListener('click', (evt) => {
      // Collapse/expand
      const collapseBtn = evt.target.closest('[data-card-widget="collapse"]');
      if (collapseBtn) {
        evt.preventDefault();
        const card = collapseBtn.closest('.card');
        if (card) this._cardOperate(card, 'toggle');
        return;
      }

      // Maximize/restore
      const maxBtn = evt.target.closest('[data-card-widget="maximize"]');
      if (maxBtn) {
        evt.preventDefault();
        const card = maxBtn.closest('.card');
        if (card) this._cardOperate(card, 'toggleMaximize');
        return;
      }

      // Refresh / loading
      const refreshBtn = evt.target.closest('[data-card-widget="refresh"]');
      if (refreshBtn) {
        evt.preventDefault();
        this.triggerResize(50);
        return;
      }

      // Flip
      const flipBtn = evt.target.closest('[data-card-widget="flip"]');
      if (flipBtn) {
        evt.preventDefault();
        const card = flipBtn.closest('.card');
        if (card) {
          const flipBox = card.querySelector('.flip-box');
          if (flipBox) flipBox.classList.toggle('active');
        }
        return;
      }

      // Remove
      const removeBtn = evt.target.closest('[data-card-widget="remove"]');
      if (removeBtn) {
        evt.preventDefault();
        const card = removeBtn.closest('.card');
        if (card) this._cardOperate(card, 'remove');
        return;
      }

      // Chat toggle (card2)
      const chatBtn = evt.target.closest('[data-shidashi-action="chat-toggle"]');
      if (chatBtn) {
        evt.preventDefault();
        const directChat = chatBtn.closest('.direct-chat');
        if (directChat) directChat.classList.toggle('direct-chat-contacts-open');
        return;
      }

      // Drawer toggle / open / close
      const drawerToggle = evt.target.closest('[data-shidashi-action="toggle-drawer"]');
      if (drawerToggle) {
        evt.preventDefault();
        this.toggleDrawer();
        return;
      }
      const drawerOpen = evt.target.closest('[data-shidashi-action="open-drawer"]');
      if (drawerOpen) {
        evt.preventDefault();
        this.openDrawer();
        return;
      }
      const drawerClose = evt.target.closest('[data-shidashi-action="close-drawer"]');
      if (drawerClose) {
        evt.preventDefault();
        this.closeDrawer();
        return;
      }
    });

    // Double-click flip-box
    document.addEventListener('dblclick', (evt) => {
      this.matchSelector(evt.target, '.flip-box', (item) => {
        const action = item.getAttribute('data-toggle') || item.getAttribute('data-bs-toggle');
        if (action === 'click') {
          item.classList.toggle('active');
        } else if (action === 'click-front') {
          item.classList.add('active');
        }
      });
    });
  }

  // ---------- Initialization ----------

  _finalize_initialization() {
    if (this._initialized) return;
    this._initialized = true;

    // Initialize sidebar
    const sidebarEl = document.querySelector('.shidashi-sidebar');
    if (sidebarEl) {
      this.sidebar = new Sidebar(sidebarEl);
      document.body.classList.add('has-sidebar');
    }

    // Detect iframe context — hide module header when embedded
    if (window.self !== window.top) {
      document.body.classList.add('in-iframe');
    }

    // Initialize iframe manager
    const iframeContainer = document.querySelector('.shidashi-content');
    if (iframeContainer) {
      this.iframeManager = new IFrameManager(iframeContainer);
    }

    // Restore theme
    const theme = this._sessionStorage.getItem(this._keyTheme);
    if (theme === 'light') {
      this.asLightMode();
    } else if (theme === 'dark') {
      this.asDarkMode();
    } else if (document.body.classList.contains('dark-mode')) {
      // Body starts with dark-mode class from R but no stored preference
      const header = document.querySelector('.shidashi-header');
      if (header) { header.setAttribute('data-bs-theme', 'dark'); }
    }

    // Dismiss preloader (was handled by AdminLTE in the original)
    this._dismissPreloader();

    // Back-to-top widget
    this._initBackToTop();

    // Card tools delegation
    this._bindCardTools();

    // Initialize custom resize handles for .resize-vertical elements
    this._initResizeHandles();

    // Bind drawer close button (the .shidashi-drawer-close inside the drawer)
    this._initDrawer();

    // ------ RAVE-specific click handlers ------

    // .rave-button click → parse rave-action JSON, send to Shiny + broadcast to iframes
    document.addEventListener('click', (evt) => {
      const raveBtn = evt.target.closest('.rave-button[rave-action]');
      if (raveBtn) {
        evt.preventDefault();
        let action = raveBtn.getAttribute('rave-action');
        if (typeof action === 'string') {
          try {
            action = JSON.parse(action);
            if (typeof action.type !== 'string') {
              console.warn('Cannot parse RAVE-action: ' + action);
              return;
            }
            this.shinySetInput('@rave_action@', {
              type: action.type,
              details: action,
              element_class: evt.target.className
            }, true, true);
          } catch (e) {
            console.warn('Cannot parse RAVE-action: ' + action);
          }
        }
        return;
      }

      // .shidashi-button click → parse shidashi-action JSON
      const shidashiBtn = evt.target.closest('.shidashi-button[shidashi-action]');
      if (shidashiBtn) {
        let action = shidashiBtn.getAttribute('shidashi-action');
        if (typeof action === 'string') {
          try {
            action = JSON.parse(action);
            if (typeof action.method === 'string' && typeof this[action.method] === 'function') {
              this[action.method].apply(this, action.args || []);
            }
          } catch (e) {}
        }
        return;
      }

      // .toggle-advance-options → toggle .rave-optional .soft-hidden
      const toggleBtn = evt.target.closest('.toggle-advance-options');
      if (toggleBtn) {
        const card = toggleBtn.closest('.card');
        if (card) {
          card.querySelectorAll('.rave-optional').forEach(el => {
            el.classList.toggle('soft-hidden');
          });
        }
        return;
      }

      // .ravedash-output-widget[data-type="standalone"] click
      const standaloneBtn = evt.target.closest('.ravedash-output-widget[data-type="standalone"]');
      if (standaloneBtn) {
        if (standaloneBtn.getAttribute('href') === '#') {
          let outputId = standaloneBtn.getAttribute('data-target');
          if (outputId && this._moduleId && outputId.startsWith(this._moduleId + '-')) {
            outputId = outputId.replace(this._moduleId + '-', '');
          }
          if (outputId) {
            this.launchStandaloneViewer(outputId);
          }
        }
        return;
      }
    });

    // Ctrl/Cmd + Enter → run_analysis
    document.addEventListener('keydown', (evt) => {
      if (evt.key === 'Enter' && (evt.ctrlKey || evt.metaKey)) {
        evt.preventDefault();
        this.shinySetInput('@rave_action@', {
          type: 'run_analysis'
        }, true, true);
      }
    });

    // Internal event for set_current_module → update standalone viewer links
    this._dummy2.addEventListener('shidashi-internal-event', (evt) => {
      if (!evt.detail || typeof evt.detail !== 'object' || !evt.detail.type) return;
      if (evt.detail.type === 'set this._raveId') {
        const outputWidgets = document.querySelectorAll('.ravedash-output-widget[data-type="standalone"]');
        for (let ii = 0; ii < outputWidgets.length; ii++) {
          const el = outputWidgets[ii];
          let outputId = el.getAttribute('data-target');
          if (typeof outputId === 'string') {
            if (this._moduleId && outputId.startsWith(this._moduleId + '-')) {
              outputId = outputId.replace(this._moduleId + '-', '');
            }
            if (outputId.length > 0) {
              const url = `?output_id=${outputId}&rave_id=${this._raveId}&module=standalone_viewer`;
              el.setAttribute('href', url);
              el.setAttribute('target', '_blank');
            }
          }
        }
      }
    });

    // Start-collapsed cards: after expand, remove start-collapsed class
    document.addEventListener('click', (evt) => {
      const collapseBtn = evt.target.closest('[data-card-widget="collapse"]');
      if (!collapseBtn) return;
      const card = collapseBtn.closest('.card.start-collapsed');
      if (!card) return;

      this.unbindAll(card);
      card.classList.remove('start-collapsed');
      this.bindAll(card);
    });

    // Theme switch checkbox
    const themeCheckbox = document.querySelector('.theme-switch-wrapper .theme-switch input[type="checkbox"]');
    if (themeCheckbox) {
      themeCheckbox.addEventListener('change', () => {
        if (this.isDarkMode()) {
          this.asLightMode();
        } else {
          this.asDarkMode();
        }
      });
    }

    // Storage listener (cross-tab session sync)
    window.addEventListener('storage', (evt) => {
      if (evt.key !== this._keyNotification) return;
      const storage_key = this._storage_key;
      const private_id = this._private_id;
      if (!storage_key || !private_id) return;

      try {
        const item = JSON.parse(this._localStorage.getItem(this._keyNotification));
        const last_saved = new Date(item.last_saved);
        if (new Date() - last_saved < this._storageDuration) {
          if (item.storage_key === storage_key && private_id !== item.private_id) {
            this.ensureShiny(() => {
              this._shiny.onInputChange('@shidashi@', this._localStorage.getItem(storage_key));
            });
          }
        }
      } catch (e) {}
    });

    // Sidebar nav links → open iframe tab
    if (this.iframeManager && sidebarEl) {
      sidebarEl.addEventListener('click', (evt) => {
        const link = evt.target.closest('.shidashi-nav-link[href]');
        if (!link) return;
        // Skip group toggles (they have child treeview)
        if (link.parentElement?.classList.contains('shidashi-nav-group')) return;
        const href = link.getAttribute('href');
        if (!href || href === '#') return;
        evt.preventDefault();
        const title = link.getAttribute('title') || link.textContent.trim();
        this.iframeManager.openTab(href, title);
      });
    }

    // Tab change listener: report active tab name to Shiny input
    // In BS5, tab events are 'shown.bs.tab' on the nav-link element
    document.body.addEventListener('shown.bs.tab', (evt) => {
      const el = evt.target;
      const tablist = el.closest('[role="tablist"]');
      if (!tablist) return;
      const cardTabs = tablist.closest('.card-tabs');
      if (!cardTabs) return;
      if (!tablist.id) return;
      const tabname = el.textContent;
      this.ensureShiny((shiny) => {
        shiny.setInputValue(tablist.id, tabname);
      });
    });

    // Report initial active tabs when Shiny becomes available
    // (queued via ensureShiny — will drain once shiny_connected is true)
    $(document).ready(() => {
      this.ensureShiny((shiny) => {
        const $tabLists = $('.card-tabs [role="tablist"]');
        for (let ii = 0; ii < $tabLists.length; ii++) {
          const pa = $tabLists[ii];
          if (pa && pa.id) {
            const activeTab = pa.querySelector('li.nav-item > .nav-link.active');
            if (activeTab) {
              shiny.setInputValue(pa.id, $(activeTab).text());
            }
          }
        }
      });
    });
  }

  _dismissPreloader() {
    const preloader = document.querySelector('.preloader');
    if (preloader && !preloader.classList.contains('preloader-hidden')) {
      preloader.classList.add('preloader-hidden');
    }
  }

  _initBackToTop() {
    const footerEl = document.querySelector('.ravedash-back-to-top');
    if (!footerEl) return;

    // ravedash generates data-toggle="tooltip" (BS3/4 style).
    // Migrate dropdown-toggle to BS5: set data-bs-toggle, fix aria.
    const dropdownToggle = footerEl.querySelector('.dropdown-toggle');
    if (dropdownToggle) {
      dropdownToggle.setAttribute('data-bs-toggle', 'dropdown');
      dropdownToggle.setAttribute('aria-haspopup', 'true');
      dropdownToggle.setAttribute('aria-expanded', 'false');

      // Initialise BS5 Dropdown if available
      if (window.bootstrap && window.bootstrap.Dropdown) {
        new bootstrap.Dropdown(dropdownToggle);
      }
    }

    // Migrate data-toggle="tooltip" → data-bs-toggle="tooltip" on all btns
    footerEl.querySelectorAll('[data-toggle="tooltip"]').forEach((el) => {
      el.setAttribute('data-bs-toggle', 'tooltip');
      el.removeAttribute('data-toggle');
      if (window.bootstrap && window.bootstrap.Tooltip) {
        new bootstrap.Tooltip(el);
      }
    });

    // Re-set the dropdown toggle back (tooltip migration may have overwritten it)
    if (dropdownToggle) {
      dropdownToggle.setAttribute('data-bs-toggle', 'dropdown');
    }

    // Populate the "Quick Access" section with shidashi-anchor items
    const menu = footerEl.querySelector('.dropdown-menu');
    const anchors = document.querySelectorAll('.shidashi-anchor');
    if (menu && anchors.length) {
      anchors.forEach((item) => {
        let itemId = item.getAttribute('id');
        if (typeof itemId !== 'string' || !itemId) {
          itemId = item.textContent.replace(/[^a-zA-Z0-9_-]/gi, '-').replace(/(--)/gi, '');
          itemId = 'shidashi-anchor-id-' + itemId;
          item.setAttribute('id', itemId);
        }
        const el = document.createElement('a');
        el.className = 'dropdown-item';
        el.href = '#' + itemId;
        el.textContent = item.textContent;
        menu.appendChild(el);
      });
    }

    // Scroll-to-top: the message button (btn-go-top) toggles the loader,
    // but if there's no rave-action we fall back to scroll-to-top.
    const gotopBtn = footerEl.querySelector('.btn-go-top:not([rave-action])');
    if (gotopBtn) {
      gotopBtn.addEventListener('click', () => this.scrollTop());
    }
  }

  // ---------- Custom drag-to-resize handles ----------

  _initResizeHandles() {
    const containers = document.querySelectorAll('.resize-vertical');
    if (!containers.length) return;

    containers.forEach((container) => {
      // Skip if already initialised
      if (container.querySelector('.shidashi-resize-handle')) return;

      const handle = document.createElement('div');
      handle.className = 'shidashi-resize-handle';
      container.appendChild(handle);

      let startY = 0;
      let startHeight = 0;

      const onMouseMove = (e) => {
        const delta = e.clientY - startY;
        const newHeight = Math.max(60, startHeight + delta);
        container.style.height = newHeight + 'px';
      };

      const onMouseUp = () => {
        handle.classList.remove('active');
        document.removeEventListener('mousemove', onMouseMove);
        document.removeEventListener('mouseup', onMouseUp);
        document.body.style.userSelect = '';
        document.body.style.cursor = '';
        this.triggerResize(50);
      };

      handle.addEventListener('mousedown', (e) => {
        e.preventDefault();
        startY = e.clientY;
        startHeight = container.offsetHeight;
        handle.classList.add('active');
        document.body.style.userSelect = 'none';
        document.body.style.cursor = 'ns-resize';
        document.addEventListener('mousemove', onMouseMove);
        document.addEventListener('mouseup', onMouseUp);
      });
    });
  }

  // ---------- Slide-out Drawer ----------

  /**
   * Initialize the drawer: bind close button.
   */
  _initDrawer() {
    const drawer = document.getElementById('shidashi-drawer');
    if (!drawer) return;
    const closeBtn = drawer.querySelector('.shidashi-drawer-close');
    if (closeBtn) {
      closeBtn.addEventListener('click', (e) => {
        e.preventDefault();
        this.closeDrawer();
      });
    }
  }

  /**
   * Open the slide-out drawer.
   * Fires an @rave_action@ event (type: 'open_drawer') so the Shiny
   * server (and all module iframes) can react to the drawer opening.
   */
  openDrawer() {
    const drawer = document.getElementById('shidashi-drawer');
    if (!drawer) return;
    if (drawer.classList.contains('open')) return; // already open
    drawer.classList.add('open');
    this.shinySetInput('@rave_action@', { type: 'open_drawer' }, true, true);
  }

  /**
   * Close the slide-out drawer.
   * Fires an @rave_action@ event (type: 'close_drawer') so the Shiny
   * server (and all module iframes) can react to the drawer closing.
   */
  closeDrawer() {
    const drawer = document.getElementById('shidashi-drawer');
    if (!drawer) return;
    if (!drawer.classList.contains('open')) return; // already closed
    drawer.classList.remove('open');
    this.shinySetInput('@rave_action@', { type: 'close_drawer' }, true, true);
  }

  /**
   * Toggle the slide-out drawer.
   * Delegates to openDrawer() / closeDrawer() so the rave action is
   * always fired exactly once per state change.
   * @param {boolean|undefined} open - Force open (true) or close (false).
   *   If undefined, toggles the current state.
   */
  toggleDrawer(open) {
    const drawer = document.getElementById('shidashi-drawer');
    if (!drawer) return;
    const shouldOpen = typeof open === 'boolean' ? open : !drawer.classList.contains('open');
    if (shouldOpen) {
      this.openDrawer();
    } else {
      this.closeDrawer();
    }
  }

  // ---------- Register Shiny message handlers ----------

  _register_shiny() {
    if (!this._shiny) {
      if (window.Shiny) {
        this._shiny = window.Shiny;
      } else {
        console.error('Cannot find window.Shiny object. Is R-shiny running?');
        return false;
      }
    }
    if (this._shiny_registered) return;
    this._shiny_registered = true;

    this.shinyHandler('click', (params) => {
      this.click(params.selector);
    });

    this.shinyHandler('box_flip', (params) => {
      this.flipBox(params.inputId);
    });

    this.shinyHandler('card_tabset_insert', (params) => {
      const added = this.tabsetAdd(params.inputId, params.title, params.body, params.active);
      if (params.notify_on_failure === true && added !== true) {
        this.createNotification({
          autohide: true,
          delay: 2000,
          title: 'Cannot create new tab',
          body: added,
          class: 'bg-warning'
        });
      }
    });

    this.shinyHandler('card_tabset_remove', (params) => {
      const removed = this.tabsetRemove(params.inputId, params.title);
      if (params.notify_on_failure === true && removed !== true) {
        this.createNotification({
          autohide: true,
          delay: 2000,
          title: 'Cannot remove tab ' + params.title,
          body: removed,
          class: 'bg-warning'
        });
      }
    });

    this.shinyHandler('card_tabset_activate', (params) => {
      const activated = this.tabsetActivate(params.inputId, params.title);
      if (params.notify_on_failure === true && activated !== true) {
        this.createNotification({
          autohide: true,
          delay: 2000,
          title: 'Cannot activate tab ' + params.title,
          body: activated,
          class: 'bg-warning'
        });
      }
    });

    this.shinyHandler('cardwidget', (params) => {
      if (params.inputId) {
        // Direct id-based lookup
        const el = document.getElementById(params.inputId);
        if (el) {
          const card = el.closest('.card') || el;
          this._cardOperate(card, params.method);
        }
      } else if (params.title) {
        // Title-based lookup via data-title attribute
        const cards = document.querySelectorAll('.card[data-title]');
        for (const c of cards) {
          if (c.getAttribute('data-title') === params.title) {
            this._cardOperate(c, params.method);
            break;
          }
        }
      }
    });

    this.shinyHandler('card2widget', (params) => {
      this.toggleCard2(params.selector);
    });

    this.shinyHandler('toggle_drawer', (params) => {
      // params.open: true → open, false → close, undefined → toggle
      if (params.open === true) {
        this.openDrawer();
      } else if (params.open === false) {
        this.closeDrawer();
      } else {
        this.toggleDrawer();
      }
    });

    this.shinyHandler('show_notification', (params) => {
      this.createNotification(params);
    });

    this.shinyHandler('clear_notification', (params) => {
      this.clearNotification(params.selector);
    });

    this.shinyHandler('set_progress', (params) => {
      this.setProgress(params.outputId, params.value, params.max || 100, params.description);
    });

    this.shinyHandler('make_scroll_fancy', (params) => {
      // No-op: using CSS native scrollbars instead of OverlayScrollbars
      // Keeping handler registered so R calls don't error
    });

    this.shinyHandler('cache_session_input', (params) => {
      this.sessionData = params.inputs;
      this.broadcastSessionData(params.shared_id, params.private_id);
    });

    this.shinyHandler('get_theme', (params) => {
      this._reportTheme();
    });

    this.shinyHandler('reset_output', (params) => {
      const el = document.getElementById(params.outputId);
      if (el && el.parentElement) {
        this.ensureShiny((shiny) => {
          const $parentEl = $(el.parentElement);
          Object.keys(shiny.outputBindings.bindingNames).forEach((key) => {
            const binding = shiny.outputBindings.bindingNames[key].binding;
            if (binding && typeof binding.find === 'function') {
              $(binding.find($parentEl)).each(function() {
                if (this.id === el.id) {
                  binding.renderError(el, {
                    message: params.message || '',
                    type: 'shiny-output-error-shiny.silent.error shiny-output-error-validation'
                  });
                }
              });
            }
          });
        });
      }
    });

    // --- Additional handlers used by ravedash ---

    this.shinyHandler('set_current_module', (params) => {
      this._moduleId = params.module_id;
      this._raveId = params.rave_id;
      this._active_module = params.module_id;
      // Dispatch internal event for standalone viewer link updates
      this._dummy2.dispatchEvent(new CustomEvent('shidashi-internal-event', {
        detail: {
          type: 'set this._raveId',
          value: params.rave_id
        }
      }));
      if (this.sidebar && params.module_id) {
        this.sidebar.setActiveByModule(params.module_id);
      }
      if (this.iframeManager && params.module_id) {
        this.iframeManager.openTabByModule(params.module_id, params.title);
      }
    });

    this.shinyHandler('hide_header', (params) => {
      this.addClass('body', 'navbar-hidden');
      this.notifyParent('addClass', ['body', 'navbar-hidden']);
    });

    this.shinyHandler('show_header', (params) => {
      this.removeClass('body', 'navbar-hidden');
      this.notifyParent('removeClass', ['body', 'navbar-hidden']);
    });

    this.shinyHandler('open_url', (params) => {
      if (params && typeof params === 'object' && typeof params.url === 'string') {
        const target = params.target || '_blank';
        this.openURL(params.url, target);
      }
    });

    this.shinyHandler('shutdown_session', (params) => {
      // Close the window or navigate away
      if (params.url) {
        window.location.href = params.url;
      } else {
        window.close();
      }
    });

    this.shinyHandler('open_iframe_tab', (params) => {
      if (this.iframeManager) {
        this.iframeManager.openTab(params.url, params.title || 'Module');
      }
    });

    this.shinyHandler('set_html', (params) => {
      if (params.selector) {
        const els = document.querySelectorAll(params.selector);
        els.forEach(el => {
          if (params.content !== undefined) {
            this.unbindAll(el, false);
            if (params.is_text) {
              el.textContent = params.content;
            } else {
              el.innerHTML = params.content;
            }
            this.bindAll(el, false);
          }
        });
      }
    });

    this.shinyHandler('accordion', (params) => {
      if (!params.selector) return;
      const el = document.querySelector(params.selector);
      if (!el) return;
      const method = params.method || 'toggle';
      if (method === 'expand') {
        if (el.classList.contains('collapsed')) { el.click(); }
      } else if (method === 'collapse') {
        if (!el.classList.contains('collapsed')) { el.click(); }
      } else {
        el.click();
      }
    });

    this.shinyHandler('add_class', (params) => {
      if (params.selector && params.class) {
        this.addClass(params.selector, params.class);
      }
    });

    this.shinyHandler('remove_class', (params) => {
      if (params.selector && params.class) {
        this.removeClass(params.selector, params.class);
      }
    });

    // Hide toasts with .hide-on-shiny-idle class when Shiny goes idle
    $(document).on('shiny:idle', () => {
      document.querySelectorAll('.toast.hide-on-shiny-idle').forEach(el => {
        if (window.bootstrap?.Toast) {
          const instance = bootstrap.Toast.getInstance(el);
          if (instance) { instance.hide(); return; }
        }
        el.remove();
      });
    });
  }
}

// ============================================================================
// Bootstrap
// ============================================================================

// Create global instance immediately
window.shidashi = new ShidashiApp();

// Initialize when DOM is ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', _init);
} else {
  _init();
}

function _init() {
  registerOutputBindings();
  window.shidashi._finalize_initialization();

  // Register shiny handlers when shiny connects
  // NOTE: 'shiny:connected' is a jQuery custom event, must use $(document).on()
  $(document).on('shiny:connected', () => {
    // 1. Register all message handlers first
    window.shidashi._register_shiny();
    // 2. NOW mark as connected and drain the queue
    window.shidashi.shiny_connected = true;
    window.shidashi.ensureShiny();
    // 3. Dismiss preloader now that Shiny is ready
    window.shidashi._dismissPreloader();
  });
}

// Highlight.js initialization (if loaded)
if (window.hljs) {
  window.hljs.configure({ languages: [] });
  if (typeof window.hljs.highlightAll === 'function') {
    window.hljs.highlightAll();
  } else if (typeof window.hljs.initHighlightingOnLoad === 'function') {
    window.hljs.initHighlightingOnLoad();
  }
}
