import $ from 'jquery';
import {
  OverlayScrollbars,
  ScrollbarsHidingPlugin,
  SizeObserverPlugin,
  ClickScrollPlugin
} from 'overlayscrollbars';

OverlayScrollbars.plugin([
  SizeObserverPlugin,
  ScrollbarsHidingPlugin,
  ClickScrollPlugin
]);

const default_scroll_opt = {
  /*
  autoUpdate           : null,
  autoUpdateInterval   : 330,
  sizeAutoCapable      : true,
  scrollbars : {
    visibility       : "auto",
    autoHide         : "move",
    autoHideDelay    : 800,
    dragScrolling    : true,
    clickScrolling   : true,
    touchSupport     : true,
  },
  textarea : {
    dynWidth       : false,
    dynHeight      : true,
    inheritedAttrs : ["style", "class"]
  }*/
  paddingAbsolute             : true,
  showNativeOverlaidScrollbars: false,
  update: {
    elementEvents     : [['img', 'load']],
    debounce          : [0, 330],
    attributes        : null,
    ignoreMutation    : null,
  },
  scrollbars: {
    theme             : 'os-theme-dark',
    visibility        : 'auto',
    autoHide          : 'move',
    autoHideDelay     : 800,
    dragScroll        : true,
    clickScroll       : true,
    pointers          : ['mouse', 'touch', 'pen'],
  },
};

class Shidashi {

  constructor (Shiny){
    // Insert build version here
    this.build = { version: '1.0', date: '2024-03-23 22:45:32 EDT' };
    this._keep_alive = true;
    this._moduleId = undefined;
    this._raveId = undefined;
    this._active_module = undefined;
    this._shiny_inactive = false;
    this._shiny_callstacks = [];
    this._shiny = Shiny;
    this.shiny_connected = false;
    this.$window = $(window);
    this.$document = $(document);
    this.$body = $("body");
    this.$aside = $("aside");
    this.$navIfarme = $(".navbar-nav-iframe");
    this.$iframeWrapper = $(".content-wrapper.iframe-mode");

    this._dummy = document.createElement("div");
    this._dummy2 = document.createElement("div");
    this._dummyLink = document.createElement("a");
    this._dummyLink.setAttribute("target", "_blank");

//shidashi.$body.append("aaaaa")
    this._localStorage = window.localStorage;
    this._sessionStorage = window.sessionStorage;
    this._keyPrefix = "shidashi-session-";
    this._keyNotification = "shidashi-session";
    this._keyTheme = "shidashi-theme";
    this._listeners = {};
    this._storageDuration = 1000 * 60 * 60 * 24; // 1000 days
    this.sessionData = {};
    this._bodyClasses = [];
    this.variableBodyClasses = ["scroller-not-top", "navbar-hidden"];
    this.scroller = this.makeFancyScroll(
      "body:not(.overflow-hidden)",
      {
        overflow : {
            x : "hidden",
            y : "scroll"
        },
      },
      {
        scroll: ( instance, event ) => {
          this._mainScrollCallback( instance );
        }
      }
    );
  }

  _mainScrollCallback(scrollers) {
    // FIXME: Hide navbar when scrolled down to show more space. It's tricky
    return;
    let isTop, param;
    if(Array.isArray(scrollers)) {
      isTop = scrollers
        .map((scroller, ii) => {
          param = scroller.scroll();
          return(param.position.y);
        })
        .filter(v => {
          return(v == 0);
        })
        .length;
    } else {
      param = scrollers.scroll();
      isTop = param.position.y == 0;
    }

    if(isTop) {
      this.notifyParent(
        "removeClass", [
          "body",
          "scroller-not-top"
        ]
      );
      this.removeClass("body", "scroller-not-top");
    } else {
      this.notifyParent(
        "addClass", [
          "body",
          "scroller-not-top"
        ]
      );
      this.addClass("body", "scroller-not-top");
    }
  }

  openURL(url, target = "_blank") {
    console.debug(`Opening ${url}`);
    this._dummyLink.setAttribute("target", target);
    this._dummyLink.setAttribute("href", url);
    this._dummyLink.click();
    this._dummyLink.innerHTML = "A new window with given link has been opened. If you haven't seen it, please click here. (This notification automatically closes in 10 seconds.)"
    this.createNotification({
      title: 'Opening the link',
      fixed: true,
      autoremove: true,
      autohide: true,
      delay: 10000,
      icon: "fas fa-link",
      subtitle: "Link created",
      close: true,
      body: this._dummyLink
    })
  }

  openIFrameTab(url, title, more = {}, target = "_blank") {
    if( !this.$body.hasClass("parent-frame") ) {
      if( window.parent === window ) {
        this.openURL( url, target );
      } else {
        this.notifyParent("openIFrameTab", [url, title, target, more]);
      }
      return;
    }
    const adminLTEIFrame = $('.content-wrapper').data('adminLTEIframeHandler');
    if( !adminLTEIFrame ) {
      return;
    }
    const $title = document.createElement("p");
    $title.text( title );

    const $link = document.createElement("a");
    $link.setAttribute("href", url);
    $link.setAttribute("target", target);
    $link.setAttribute("title", title);
    for(let k in more) {
      $link.setAttribute(k, more[k]);
    }
    $link.appendChild( $title );
    adminLTEIFrame.openTabSidebar($link);
  }

  launchStandaloneViewer(outputId) {
    const url = `?output_id=${ outputId }&rave_id=${ this._raveId }&module=standalone_viewer`;
    this.openURL(url);
  }

  ensureShiny(then){
    if(!this._shiny){
      this._shiny = window.Shiny;
    }
    if( typeof(then) === "function" ){
      this._shiny_callstacks.push(then);
    }
    if(document.readyState && document.readyState === "complete" &&
      this._shiny && this.shiny_connected) {

      while(this._shiny_callstacks.length > 0) {
        const f = this._shiny_callstacks.shift();
        try{
          f(this._shiny);
        }catch(e){
          console.warn(e);
        }
      }
    } else {
      console.debug(`Shiny is not connected, defering (${ this._shiny_callstacks.length }) requests...`);
    }
  }

  bindAll( el, ensure = true ) {
    const b = (shiny) => {
      shiny.bindAll( el );
      // also check tabsets
      const $tabLists = $( el ).find('.card-tabs [role="tablist"]')
      for( let ii = 0; ii < $tabLists.length; ii++ ) {
        const pa = $tabLists[ ii ];
        if(pa && pa.id) {
          const activeTab = pa.querySelector("li.nav-item > .nav-link.active");
          if( activeTab ) {
            shiny.setInputValue( pa.id, $(activeTab).text() );
          }
        }
      }

    };

    if( ensure || this._shiny ) {
      this.ensureShiny(b);
    }
  }
  unbindAll( el, ensure = true ) {
    const ub = (shiny) => {
      shiny.unbindAll( el );
    };
    if( ensure || this._shiny ) {
      this.ensureShiny( ub );
    }
  }

  // localStorage to save input data
  fromLocalStorage(key, defaultIfNotFound, ignoreDuration = false){
    try {
      const item = JSON.parse(this._localStorage.getItem(key));
      item.last_saved = new Date(item.last_saved);
      item._key = key;
      if( !ignoreDuration ){
        const now = new Date();
        if((now - item.last_saved) > this._storageDuration) {
          // item expired
          console.debug("Removing expired key: " + key);
          this._localStorage.removeItem(key);
        } else {
          return(item);
        }
      } else {
        return(item);
      }
    } catch (e) {
      console.debug("Removing corrupted key: " + key);
      this._localStorage.removeItem(key);
    }
    if(defaultIfNotFound === true){
      return({
        inputs : {},
        last_saved: new Date(),
        last_edit: this._private_id,
        inputs_changed: [],
        _key: key
      });
    } else {
      return (defaultIfNotFound);
    }

  }

  async cleanLocalStorage(maxEntries = 100) {
    // Clean the localStorage
    const items = [];
    for(let key in this._localStorage){
      if(key.startsWith(this._keyPrefix)){
        const item = this.fromLocalStorage(key);
        if(maxEntries && item){
          items.push( item );
        }
      }
    }

    if(items.length && items.length > maxEntries){
      items.sort((v1, v2) => { return(v1.last_saved > v2.last_saved); });
      items.splice(items.length - maxEntries);
      items.forEach((item) => {
        this._localStorage.removeItem(item._key);
      });
    }
  }

  _setSharedId(shared_id) {
    if(typeof(this._shared_id) !== "string" && typeof(shared_id) === "string"){
      this._shared_id = shared_id;
      this._storage_key = this._keyPrefix + this._shared_id;
    }
    return this._storage_key;
  }
  _setPrivateId(private_id) {
    if(typeof(this._private_id) !== "string"){
      if(typeof(private_id) === "string"){
        this._private_id = private_id;
      } else {
        this._private_id = Math.random().toString(16).substr(2, 8);
      }
    }
    return this._private_id;
  }

  broadcastSessionData(shared_id, private_id){
    const storage_key = this._setSharedId(shared_id);
    if(!storage_key){ return; }
    const private_id_ = this._setPrivateId(private_id);

    const keys_changed = Object.keys(this.sessionData);
    if(!keys_changed.length){
      return;
    }

    const now = new Date();

    // load up from localStorage
    const stored = this.fromLocalStorage(storage_key, true, true);
    stored.last_saved = now;
    stored.last_edit = private_id_;
    stored.inputs_changed = keys_changed;
    for(let k in this.sessionData){
      stored.inputs[k] = this.sessionData[k];
    }
    this._localStorage.setItem(storage_key, JSON.stringify(stored));
    this._localStorage.setItem(this._keyNotification, JSON.stringify({
      "storage_key" : storage_key,
      "private_id": private_id_,
      "last_saved": now
    }));

  }
  broadcastEvent(type, message = {}) {
    const event = new CustomEvent("shidashi-event-" + type, {
      "detail": message
    });
    this._dummy.dispatchEvent(event);
    // also send to shiny
    this.ensureShiny((shiny) => {
      if(typeof(shiny.setInputValue) !== "function"){ return; }
      shiny.setInputValue("@shidashi_event@", {
        type: type,
        message: message,
        shared_id: this._shared_id,
        private_id: this._private_id
      });
    });
  }
  registerListener(type, callback, replace = true) {
    const event_str = "shidashi-event-" + type;
    if(replace){
      const old_function = this._listeners[type];
      if(typeof(old_function) === "function"){
        this._dummy.removeEventListener(event_str, old_function);
      }
    }
    if(typeof(callback) === "function"){
      const cb_ = (evt) => {
        return(callback(evt.detail));
      };
      this._dummy.addEventListener(event_str, cb_);
      this._listeners[type] = cb_;
    }
  }

  _col2Hex(color){
    let col = color.trim();
    if(col.length < 4){ return("#000000"); }
    if(col[0] === "#"){
      if(col.length === 7){ return(col); }
      col = "#"+col[1]+col[1]+col[2]+col[2]+col[3]+col[3];
      return(col);
    }
    let parts = col.match(/rgb[a]{0,1}\((\d+),\s*(\d+),\s*(\d+)[\),]/);
    delete(parts[0]);
    for (var i = 1; i <= 3; ++i) {
      parts[i] = parseInt(parts[i]).toString(16);
      if (parts[i].length == 1) parts[i] = '0' + parts[i];
    }
    col = '#' + parts.join('');
    return(col);
  }
  _reportTheme(mode){
    if(typeof(mode) !== "string"){
      const isDark = this.isDarkMode();
      mode = isDark ? "dark": "light";
    }
    const $card_body = $(".card, .info-box");
    let bgcolor = this._col2Hex(this.$body.css("background-color"));
    if($card_body.length){
      bgcolor = this._col2Hex($($card_body[0]).css("background-color"));
    } else if (mode === "dark"){
      bgcolor = "#343a40";
    }
    this.broadcastEvent("theme.changed", {
      mode: mode,
      background: bgcolor,
      foreground: this._col2Hex(this.$body.css("color"))
    });
  }

  notifyIframes(method, args){
    if(this.$iframeWrapper.length){
      const $iframes = this.$iframeWrapper.find("iframe");
      $iframes.each((_, iframe) => {
        try {
          if(iframe.contentWindow.shidashi){
            iframe.contentWindow.shidashi[method](...args);
          }
        } catch (e) {}
      });
    }
  }

  notifyParent(method, args) {
    if(window.parent && window.parent !== window) {
      if( window.parent.shidashi ) {
        window.parent.shidashi[method](...args);
      }
    }
  }

  // status

  // theme-mode
  asLightMode(){
    this.$body.removeClass("dark-mode");
    //this.$aside.removeClass("sidebar-dark-primary")
    //  .addClass("sidebar-light-primary");
    this.$navIfarme.removeClass("navbar-dark")
      .addClass("navbar-light");
    if(this.$iframeWrapper.length){
      this._sessionStorage.setItem(
        this._keyTheme, "light"
      );
      const $iframes = this.$iframeWrapper.find("iframe");
      $iframes.each((_, iframe) => {
        if(iframe.contentWindow.shidashi){
          iframe.contentWindow.shidashi.asLightMode();
        }
      });
    }
    this._reportTheme("light");
  }

  asDarkMode(){

    this.$body.addClass("dark-mode");
    //this.$aside.removeClass("sidebar-light-primary")
    //  .addClass("sidebar-dark-primary");
    this.$navIfarme.removeClass("navbar-light")
      .addClass("navbar-dark");
    if(this.$iframeWrapper.length){
      this._sessionStorage.setItem(
        this._keyTheme, "dark"
      );
      const $iframes = this.$iframeWrapper.find("iframe");
      $iframes.each((_, iframe) => {
        if(iframe.contentWindow.shidashi){
          iframe.contentWindow.shidashi.asDarkMode();
        }
      });
    }
    this._reportTheme("dark");
  }

  resumeStatus(parentShidashi) {
    if(!parentShidashi) {
      return;
    }
    if(parentShidashi._active_module !== this._moduleId){
      return;
    }

    console.debug(`Resuming status - ${ this._moduleId }`);
    // body classes
    this.variableBodyClasses.forEach((cls) => {
      if( this._bodyClasses.contains(cls) ) {
        parentShidashi.addClass("body", cls);
      } else {
        parentShidashi.removeClass("body", cls);
      }
    });

  }

  // Trigger actions
  click(selector) {
    if(!selector || selector === ''){ return; }
    const el = $(selector);
    if(!el.length){ return; }
    el.click();
  }

  triggerResize(timeout) {
    if( timeout ){
      setTimeout(() => {
        this.triggerResize();
      }, timeout);
    } else {
      this.$window.trigger("resize");
      this.unbindAll( this._dummy2 );
    }

  }

  // tabset
  tabsetAdd(inputId, title, body, active = true){
    let el = document.getElementById(inputId);
    let elbody = document.getElementById(inputId + "Content");
    if(!el){ return("Cannot find tabset with given settings."); }
    if(!elbody){ return("Cannot find tabset with given settings."); }

    el = $(el);

    // check if title existed
    const existing_items = el.children(".nav-item.nav-tab-header");
    if(existing_items.length){
      const existing_title = existing_items.children(".nav-link")
        .toArray()
        .map((v) => {return( $(v).text() );});
      if(existing_title.includes(title)){
        return("A tab with title '" + title + "' already exists.");
      }
    }

    // this._shiny.unbindAll(el);

    const tabId = Math.random().toString(16).substr(2, 8);

    // Create header
    const header_item = document.createElement("li");
    header_item.className = "nav-item nav-tab-header";
    const header_a = document.createElement("a");
    header_a.className = "nav-link";
    header_a.setAttribute("href", `#${ inputId }-${tabId}`);
    header_a.setAttribute("id", `${ inputId }-${tabId}-tab`);
    header_a.setAttribute("data-toggle", "tab");
    header_a.setAttribute("role", "tab");
    header_a.setAttribute("aria-controls", `${ inputId }-${tabId}`);
    header_a.setAttribute("aria-selected", "false");
    // header_a.innerText = title;
    $(header_a).text( title );

    header_item.appendChild(header_a);

    // add to header

    if(existing_items.length > 0){
      existing_items.last().after(header_item);
    }

    // body
    const body_el = document.createElement("div");
    body_el.className = "tab-pane fade";
    body_el.setAttribute("id", `${ inputId }-${tabId}`);
    body_el.setAttribute("role", "tabpanel");
    // body_el.setAttribute("tab-index", tabId);
    body_el.setAttribute("aria-labelledby", `${ inputId }-${tabId}-tab`);
    body_el.innerHTML = body;
    elbody.appendChild(body_el);


    this.bindAll( $(elbody) );

    if(active){
      return(this.tabsetActivate(inputId, title));
    }

    return(true);

  }

  tabsetRemove(inputId, title) {
    let el = document.getElementById(inputId);
    let elbody = document.getElementById(inputId + "Content");
    if(!el){ return("Cannot find tabset with given settings."); }
    if(!elbody){ return("Cannot find tabset with given settings."); }

    el = $(el);

    // check if title existed
    const existing_items = el.children(".nav-item.nav-tab-header");
    if(!existing_items.length) {
      return("Tab with title '" + title + "' cannot be found.");
    }
    el = existing_items.children(".nav-link");
    let activate = false;
    let remove_idx = 0;
    const existing_title = el.toArray()
      .map((v, i) => {
        if( $(v).text() === title ) {
          // remove this tab
          remove_idx = i;
          const rem = $(el[i]);
          const tabid = rem.attr("aria-controls");
          const tab = $("#" + tabid);
          const is_active = rem.attr("aria-selected");
          this.unbindAll( tab );
          rem.parent().remove();
          tab.remove();
          if(is_active === "true"){
            activate = true;
          }
        }
        return( $(v).text() );
      });
    if(!existing_title.includes(title)){
      return("A tab with title '" + title + "' cannot be found.");
    }
    if(activate && existing_items.length > 1){
      let active_tab;
      if(remove_idx - 1 >= 0){
        active_tab = existing_items[remove_idx - 1];
      } else {
        active_tab = existing_items[remove_idx + 1];
      }
      $(active_tab).children("a.nav-link").click();
    }
    return(true);
  }

  tabsetActivate(inputId, title) {
    let el = document.getElementById(inputId);
    let elbody = document.getElementById(inputId + "Content");
    if(!el){ return("Cannot find tabset with given settings."); }
    if(!elbody){ return("Cannot find tabset with given settings."); }

    el = $(el);
    const existing_items = el.children(".nav-item.nav-tab-header");
    if(!existing_items.length) {
      return("Tab with title '" + title + "' cannot be found.");
    }

    let activated = false;
    existing_items.each((_, item) => {
      const link = $(item).children(".nav-link");
      if(link.text() === title){
        link.click();
        activated = true;
      } else {
        link.removeClass("active");
        link.attr("aria-selected", "false");
      }
    });

    if(!activated){
      return("Tab with title '" + title + "' cannot be found.");
    }
    return(true);
  }

  // card, card2, cardset...
  card(args){
    // method: expand, minimize, maximize, ...
    if( !args.method ){ return; }
    if( args.inputId ){
      $(".card#" + args.inputId).CardWidget(args.method);
    } else if (args.title){
      $(`.card[data-title='${args.title}']`).CardWidget(args.method);
    } else if (args.selector) {
      $(`.card${args.selector}`).CardWidget(args.method);
    }

  }

  toggleCard2(selector){
    $(selector).DirectChat("toggle");
  }

  accordion(args) {
    // method: expand, collapse, toggle
    let $accordionItem;
    if( args.inputId && args.title ){
      $accordionItem = $(`.card-accordion#${ args.inputId } .card-accordion-header[data-title='${args.title}']`);
    } else if (args.selector) {
      $accordionItem = $(`.card-accordion${args.selector}`);
    }
    if(!$accordionItem || !$accordionItem.length){ return; }

    if( args.method === "expand" ) {
      if( $accordionItem.hasClass("collapsed") ) {
        $accordionItem.click();
      }
    } else if ( args.method === "collapse" ) {
      if( !$accordionItem.hasClass("collapsed") ) {
        $accordionItem.click();
      }
    } else {
      $accordionItem.click();
    }
  }

  flipBox(inputId){
    let el = document.getElementById(inputId);
    if(el && el.classList.contains("flip-box")) {
      if( el.classList.contains("active") ){
        el.classList.remove("active");
      } else {
        el.classList.add("active");
      }
    }
  }

  // html css operations
  addClass(selector, cls){
    $(selector).addClass(cls);
    if( selector.startsWith("body") ) {
      this._bodyClasses = this.$body[0].classList;
    }
  }
  removeClass(selector, cls){
    $(selector).removeClass(cls);
    if( selector.startsWith("body") ) {
      this._bodyClasses = this.$body[0].classList;
    }
  }

  setInnerHtml(selector, content) {
    const $el = $(selector);
    if(!$el.length) { return; }

    this.unbindAll( $el, false );

    $el.html(content);

    this.bindAll( $el, false );
  }

  // notification
  createNotification(options) {
    // see https://adminlte.io/docs/3.1//javascript/toasts.html
    this.$document.Toasts('create', options);
  }

  clearNotification(selector) {
    $(selector || ".toast").toast("hide");
  }

  // set progressOutput
  setProgress(inputId, value, max = 100, description = null){
    if(typeof(value) !== "number" || isNaN(value)){ return; }
    const el = document.getElementById(inputId);
    if(!el){ return; }

    let v = parseInt(value / max * 100);
    if(v < 0){ v = 0; }
    if(v > 100){ v = 100; }
    $(el).find(".progress-bar").css("width", `${v}%`);
    if(typeof(description) === "string"){
      $(el)
        .find(".progress-description.progress-message")
        .text(description);
    }
  }

  // theme-mode
  isDarkMode() {
    return(this.$body.hasClass("dark-mode"));
  }

  // scroller
  makeFancyScroll(selector, options = {}, callbacks = {}) {
    // https://kingsora.github.io/OverlayScrollbars/#!documentation/options
    const dark_mode = this.isDarkMode();

    const className = options.className || (dark_mode ? "os-theme-thin-light" : "os-theme-thin-dark");

    options.className = className;

    const elems = document.querySelectorAll(selector);

    let instances = [];

    const scrollOptions = $.extend(default_scroll_opt, options);

    elems.forEach( el => {
      const instance = OverlayScrollbars({
        target: el,
      }, scrollOptions, callbacks);
      instances.push( instance );
    })
    // const instance = $(selector)
    //   .overlayScrollbars($.extend(default_scroll_opt, options))
    //   .overlayScrollbars();

    if( instances.length === 1 ) {
      instances = instances[0];
    }

    return(instances);
  }

  scrollTop(duration = 200) {
    if(this.scroller){
      // FIXME
      // this.scroller.scroll({ y : "0%" }, duration);
    }
  }

  // utils, shiny, ...
  async matchSelector(el, selector, next, strict = false) {
    const $el = $(el);
    const $els = $(selector);

    if(!$el.length || !$els.length){ return; }

    const el_ = $el[0];

    const els = $els.toArray();
    let item;
    for( let i in els ){
      item = els[i];
      if(item === el_ || (!strict && item.contains(el_))){
        if(typeof(next) === "function"){
          return(next(item));
        } else {
          return(true);
        }
      }
    }
    return;
  }

  shinyHandler(action, callback) {
    if(!this._shiny){
      if( window.Shiny ){
        this._shiny = window.Shiny;
      } else {
        console.error("Cannot find window.Shiny object. Is R-shiny running?");
        return false;
      }
    }
    this._shiny.addCustomMessageHandler("shidashi." + action, callback);
  }

  shinyOn( eventType, callback ) {
    if(!this._shiny){
      if( window.Shiny ){
        this._shiny = window.Shiny;
      } else {
        console.error("Cannot find window.Shiny object. Is R-shiny running?");
        return false;
      }
    }

    this.$document.on( eventType, ( event ) => {
      callback( event );
    })
  }

  shinySetInput(inputId, value, add_timestamp = true, children = false) {
    this.ensureShiny((shiny) => {
      if( add_timestamp ){
        value.timestamp = new Date();
      }
      value._active_module = this._active_module;
      value.parent_frame = this.$body.hasClass("parent-frame");
      shiny.setInputValue(inputId, value);

      console.debug(`[${this._private_id}] shiny input [${inputId}] <- ${ JSON.stringify(value) }`);

      if(children){

        if(this.$iframeWrapper.length){
          const $iframes = this.$iframeWrapper.find("iframe");

          const f = (shidashi) => {
            shidashi.ensureShiny((shiny) => {
              if( shiny.shinyapp.$socket ) {
                shiny.setInputValue(inputId, value);
              }
            });
          };

          $iframes.each((_, iframe) => {
            // maybe restricted due to CORS
            try {
              /* code */
              $(iframe.contentDocument).ready(() => {
                if(iframe.contentWindow.shidashi){
                  f(iframe.contentWindow.shidashi);
                }
              });
            } catch (e) {}
          });
        }

      }

    });
  }

  shinyResetOutput(outputId, message = ""){
    const el = document.getElementById(outputId);
    if(el && el.parentElement){
      this.ensureShiny(() => {
        const $pa_el = $(el.parentElement);
        Object.keys(this._shiny.outputBindings.bindingNames).forEach((key) => {
          const binding = shidashi._shiny.outputBindings.bindingNames[key].binding;
          if(binding && typeof binding === "function") {
            $(binding.find($pa_el)).each((_, el2) => {
              if($(el2)[0].id === el.id){

                binding.renderError(el, {
                  message: message,
                  type: "shiny-output-error-shiny.silent.error shiny-output-error-validation"
                });

              }
            });
          }

        });
      });
    }

  }

  // Finalize function when document is ready
  _finalize_initialization(){
    if(this._initialized){ return; }
    this._initialized = true;
    const _this = this;

    // set theme first
    const theme = this._sessionStorage.getItem(this._keyTheme);
    if( theme === "light" ){
      this.asLightMode();
    } else if( theme === "dark"){
      this.asDarkMode();
    }

    // scroll-top widget
    const gotop_el = $(".back-to-top,.ravedash-back-to-top");
    const gotop_btn = $(".back-to-top .btn-go-top,.ravedash-back-to-top .btn-go-top");
    const root_btn = $(".back-to-top .dropdown-toggle,.ravedash-back-to-top .dropdown-toggle");
    const menu = $(".back-to-top .dropdown-menu,.ravedash-back-to-top .dropdown-menu");
    const anchors = $(".shidashi-anchor");

    // Scroll-top widgets
    anchors.each((_, item) => {
      const $item = $(item);
      let item_id = $item.attr("id");
      if( typeof(item_id) !== "string" ){
        item_id = $item.text().replace(/[^a-zA-Z0-9_-]/gi, '-').replace(/(--)/gi, '');
        item_id = "shidashi-anchor-id-" + item_id;
        $item.attr("id", item_id);
      }
      const el = document.createElement("a");
      el.className = "dropdown-item";
      el.href = "#" + item_id;
      if(item.textContent) {
        el.innerText = item.textContent.trim();
      } else {
        el.innerText = item.innerText.trim();
      }
      menu.append( el );
    });
    root_btn.mouseenter(() => {
      if(root_btn.attr("aria-expanded") === "false"){
        root_btn.dropdown("toggle");
        menu.addClass("show");
        root_btn.attr("aria-expanded", "true");
      }
    });
    gotop_el.mouseleave(() => {
      if(root_btn.attr("aria-expanded") === "true"){
        root_btn.dropdown("toggle");
        menu.removeClass("show");
        root_btn.attr("aria-expanded", "false");
      }
    });

    gotop_btn.click(() => { this.scrollTop() });

    // --------------- Triggers resize -------------------------
    this.$document.on('expanded.lte.cardwidget', (evt) => {

      if(evt.target){
        const card = $(evt.target).parents(".card.start-collapsed");

        if(card.length > 0){

          setTimeout(() => {
            this.unbindAll( card );
            card.removeClass("start-collapsed");
            this.bindAll( card );
          }, 200);

        }
      }
      this.triggerResize(50);

    });
    this.$document.on('maximized.lte.cardwidget', () => {
      this.$body.addClass("card-expanded");
      this.triggerResize(50);
    });
    this.$document.on('minimized.lte.cardwidget', () => {
      this.$body.removeClass("card-expanded");
      this.triggerResize(50);
    });
    this.$document.on("loaded.lte.cardrefresh", () => {
      this.triggerResize(50);
    });

    this.$body.on("show.bs.tab", (evt) => {
      if(evt.type !== "show") { return; }
      const el = evt.target;
      const pa = el.parentNode.closest('.card-tabs [role="tablist"]');

      if(!pa || !pa.id) { return; }
      const tabname = $(el).text();

      this.ensureShiny(() => {
        this._shiny.setInputValue(pa.id, tabname);
      });
    })


    // -------------- Documentation ready hook !!! ------------
    this.$document.ready(() => {
      this.ensureShiny((shiny) => {

        // report active tab to shiny
        const $tabLists = $( '.card-tabs [role="tablist"]' );
        for( let ii = 0; ii < $tabLists.length; ii++ ) {
          const pa = $tabLists[ ii ];
          if(pa && pa.id) {
            const activeTab = pa.querySelector("li.nav-item > .nav-link.active");
            if( activeTab ) {
              shiny.setInputValue(pa.id, $(activeTab).text());
            }
          }
        }


      });
    });

    this._dummy2.addEventListener("shidashi-internal-event", (evt) => {
      window.eeevt = evt;
      if( !evt.detail || typeof evt.detail !== "object" || !evt.detail.type ) { return; }

      switch (evt.detail.type) {
        case "set this._raveId":
          const $output_widgets = $( '.ravedash-output-widget[data-type="standalone"]' );
          for( let ii = 0 ; ii < $output_widgets.length ; ii++ ) {
            const el = $output_widgets[ ii ];
            let outputId = el.getAttribute("data-target");
            if( typeof outputId === "string" ) {
              if( outputId.startsWith(this._moduleId + "-") ) {
                outputId = outputId.replace(this._moduleId + "-", "");
              }
              if( outputId.length > 0 ) {
                const url = `?output_id=${ outputId }&rave_id=${ this._raveId }&module=standalone_viewer`;
                el.setAttribute("href", url);
                el.setAttribute("target", "_blank");
              }
            }
          }
          break;
        default:
      }
    });

    // --------------- Notification system -----------
    this.$body.on('show.bs.toast', (evt)=>{
      this.bindAll( $(evt.target) );
    });
    this.$body.on('hide.bs.toast', (evt)=>{
      this.unbindAll( $(evt.target) );
    });

    // --------------- Fancy scroll ---------------
    this.makeFancyScroll(".fancy-scroll-y:not(.overflow-hidden,.screen-height), .overflow-y-auto", {
        overflow : {
            x : "hidden",
            y : "scroll"
        }
      });

    this.makeFancyScroll(
      ".screen-height.overflow-y-scroll",
      {
        overflow : {
            x : "hidden",
            y : "scroll"
        }
      },
      {
        scroll: ( instance, event ) => {
          this._mainScrollCallback( instance );
        }
      }
    );


    this.makeFancyScroll(
      ".resize-vertical",
      {
        resize: "vertical",
        overflow : {
            x : "hidden",
            y : "scroll"
        },
      },
      {
        updated: (() => {
          let sizeChanged = 0;
          return ( instance, args ) => {
            if( args && args.updateHints.sizeChanged ) {
              let sizeChanged_ = sizeChanged + 1;
              sizeChanged = sizeChanged_;
              setTimeout(() => {
                if( sizeChanged === sizeChanged_ ) {
                  sizeChanged = 0;
                  this.triggerResize();
                }
              }, 300);
            }
          };
        })()
      }
    );
    this.makeFancyScroll(
      ".fancy-scroll-x",
      {
        overflow : {
          x : "scroll",
          y : "hidden"
        }
      }
    );
    this.makeFancyScroll(
      ".fancy-scroll, .overflow-auto",
      {
        overflow : {
          x : "scroll",
          y : "scroll"
        }
      }
    );

    // register listener
    window.addEventListener('storage', (evt) => {
        if(evt.key !== this._keyNotification){ return; }

        const storage_key = this._storage_key;
        const private_id = this._private_id;

        if(!storage_key || !private_id){ return; }

        // When local storage changes
        try {
          const item = JSON.parse(this._localStorage.getItem(this._keyNotification));
          const last_saved = new Date(item.last_saved);
          if(new Date() - last_saved < this._storageDuration){
            if(item.storage_key === storage_key) {
              if(private_id !== item.private_id){
                this.ensureShiny((shiny) => {
                  shiny.onInputChange("@shidashi@", this._localStorage.getItem(storage_key));
                });
              }
            }
          }
        } catch (e) {}
      });

    $(".theme-switch-wrapper .theme-switch input[type='checkbox']")
      .change((_) => {
        if(this.isDarkMode()){
          this.asLightMode();
        } else {
          this.asDarkMode();
        }
      });

    $(".shidashi-button").click(function(_){
      let el = this;
      let action = el.getAttribute('shidashi-action');
      if(typeof action === "string"){
        action = JSON.parse(action);
        if( typeof action.method === "string" &&
            typeof _this[action.method] === "function" ){
          _this[action.method].apply(_this, action.args);
        }
      }
    });

    $('.rave-button').click(function(evt){
      let el = this;
      try {
        // If the rave-button is removed from classlist, then do nothing
        if( !el.classList.contains("rave-button") ) { return; }
      } catch (e) {}
      let action = el.getAttribute("rave-action");
      if(typeof action === "string"){
        try {
          action = JSON.parse(action);

          if( typeof action.type !== "string" ) {
            console.warn("Cannot parse RAVE-action: " + action);
            return;
          }
          // check if body has parent-frame class
          _this.shinySetInput("@rave_action@", {
            type: action.type,
            details: action,
            element_class: evt.target.className
          }, true, true);

        } catch (e) {
          console.warn("Cannot parse RAVE-action: " + action);
        }
      }
    });

    this.$document.on("click", (evt) => {

      this.matchSelector(
        evt.target,
        '.card-tools .btn-tool[data-card-widget="refresh"]',
        () => {
          this.triggerResize(50);
        }
      );


      this.matchSelector(
        evt.target,
        '.ravedash-output-widget[data-type="standalone"]',
        (el) => {
          if( el.getAttribute("href") === "#" ) {
            let outputId = el.getAttribute("data-target");
            if( outputId.startsWith(this._moduleId + "-") ) {
              outputId = outputId.replace(this._moduleId + "-", "");
            }
            this.launchStandaloneViewer(outputId);
          }
        }
      );

      this.matchSelector(
        evt.target,
        '.card-tools .btn-tool[data-card-widget="flip"]',
        (el) => {
          const $card = $(el).parents(".card");
          if(!$card.length){ return; }
          $($card[0]).find(".card-body .flip-box").toggleClass("active");
        }
      );

      this.matchSelector(
        evt.target,
        '.toggle-advance-options',
        (el) => {
          const $card = $(el).parents(".card");
          if(!$card.length){ return; }
          $($card[0]).find(".rave-optional").toggleClass("soft-hidden");
        }
      );

    });

    this.$document.on("dblclick", (evt) => {

      this.matchSelector(
        evt.target,
        '.flip-box',
        (item) => {
          const $el = $(item);
          const action = $el.attr("data-toggle");
          if(action === "click"){
            $el.toggleClass("active");
          } else if (action === "click-front"){
            $el.addClass("active");
          }
        }
      );

    });

    this.$document.on("keydown", (evt) => {
      if(evt.key === "Enter" && (evt.ctrlKey || evt.metaKey)) {
        evt.preventDefault();
        this.shinySetInput("@rave_action@", {
          type: "run_analysis"
        }, true, true);
      }
    });

  }

  _register_shiny() {
    if(!this._shiny){
      if( window.Shiny ){
        this._shiny = window.Shiny;
      } else {
        console.error("Cannot find window.Shiny object. Is R-shiny running?");
        return false;
      }
    }
    if(this._shiny_registered) { return; }
    this._shiny_registered = true;

    this.shinyHandler("set_current_module", (params) => {
      this._moduleId = params.module_id;
      this._raveId = params.rave_id;
      this._dummy2.dispatchEvent(new CustomEvent("shidashi-internal-event", {
        "detail": {
          "type" : "set this._raveId",
          "value": params.rave_id
        }
      }))
    });

    this.shinyHandler("click", (params) => {
      this.click(params.selector);
    });
    this.shinyHandler("box_flip", (params) => {
      this.flipBox(params.inputId);
    });

    this.shinyHandler("card_tabset_insert", (params) => {
      const added = this.tabsetAdd( params.inputId, params.title,
                                    params.body, params.active );
      if(params.notify_on_failure === true && added !== true){
        this.createNotification({
          "autohide": true,
          "delay" : 2000,
          "title" : "Cannot create new tab",
          "body"  : added,
          "class" : "bg-warning"
        });
      }
    });
    this.shinyHandler("card_tabset_remove", (params) => {
      const removed = this.tabsetRemove( params.inputId, params.title );
      if(params.notify_on_failure === true && removed !== true){
        this.createNotification({
          "autohide": true,
          "delay" : 2000,
          "title" : "Cannot remove tab " + params.title,
          "body"  : removed,
          "class" : "bg-warning"
        });
      }
    });
    this.shinyHandler("card_tabset_activate", (params) => {
      const activated = this.tabsetActivate( params.inputId, params.title );
      if(params.notify_on_failure === true && activated !== true){
        this.createNotification({
          "autohide": true,
          "delay" : 2000,
          "title" : "Cannot activate tab " + params.title,
          "body"  : activated,
          "class" : "bg-warning"
        });
      }
    });

    this.shinyHandler("cardwidget", (params) => {
      this.card(params);
    });
    this.shinyHandler("card2widget", (params) => {
      this.toggleCard2(params.selector);
    });
    this.shinyHandler("accordion", (params) => {
      this.accordion(params);
    });

    this.shinyHandler("add_class", (params) => {
      this.addClass(params.selector, params.class);
    });
    this.shinyHandler("remove_class", (params) => {
      this.removeClass(params.selector, params.class);
    });
    this.shinyHandler("set_html", (params) => {
      this.setInnerHtml(params.selector, params.content);
    });

    this.shinyHandler("show_notification", (params) => {
      this.createNotification(params);
    });
    this.shinyHandler("clear_notification", (params) => {
      this.clearNotification(params.selector);
    });

    this.shinyHandler("set_progress", (params) => {
      this.setProgress(params.outputId, params.value,
        params.max || 100, params.description);
    });

    this.shinyHandler("make_scroll_fancy", (params) => {
      if(!params.selector || params.selector === ''){ return; }
      this.makeFancyScroll(
        params.selector,
        params.options || {}
      );
    });

    this.shinyHandler("cache_session_input", (params) => {
      this.sessionData = params.inputs;
      this.broadcastSessionData(params.shared_id, params.private_id);
    });

    this.shinyHandler("get_theme", (_) => {
      this._reportTheme();
    });

    this.shinyHandler("reset_output", (params) => {
      this.shinyResetOutput(params.outputId, params.message || "");
    });

    this.shinyHandler("hide_header", (params) => {
      this.addClass("body", "navbar-hidden");
      this.notifyParent("addClass", [
        "body", "navbar-hidden"
      ])
    });
    this.shinyHandler("show_header", (params) => {
      this.removeClass("body", "navbar-hidden");
      this.notifyParent("removeClass", [
        "body", "navbar-hidden"
      ])
    });

    this.shinyHandler("shutdown_session", (params) => {
      console.log("Shutting down RAVE...")
      window.close();
    });

    this.shinyHandler("open_url", (params) => {
      if(
        params && typeof params === "object" &&
        typeof params.url === "string" ) {

        const target = params.target || "_blank";
        this.openURL(params.url, "_blank");
      }

    });

    this.shinyHandler("open_iframe_tab", (params) => {
      if(
        params && typeof params === "object" &&
        typeof params.url === "string" ) {

        const title = params.title || "Untitled";
        const target = params.target || "_blank";
        const more = params.more || {};
        this.openIFrameTab(params.url, title, target, more);
      }

    });


    this.shinyOn("shiny:idle", (e) => {
      $(".toast.hide-on-shiny-idle").toast("hide");
    })




    // keep alive
    /*let alive_counter = 0;
    const keep_alive = () => {
      if( this._keep_alive ) {
        alive_counter++;
        this._shiny.setInputValue(".__shidashi_keep_alive_signal__.", alive_counter);
      }
      // send signal to R session every other 25 seconds
      setTimeout(keep_alive, 25000);
    };
    keep_alive();
    */

  }
}


export { Shidashi }
