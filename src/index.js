import $ from 'jquery';
// import 'bootstrap';
// import 'admin-lte/dist/js/adminlte.js';
import './js/AdminLTE/AdminLTE.js'
// import "admin-lte/dist/css/adminlte.css";
// import "./css/OverlayScrollbars.css";
import 'overlayscrollbars/overlayscrollbars.css';
import "./scss/shidashi.scss";
import { registerProgressOutput } from './js/shiny-progress.js';
import { registerClipboardOutput } from './js/shiny-clipboard.js';
import { registerHighlightJS } from './js/import-highlightjs.js';
import { Shidashi } from './js/class-shidashi.js';
// import "./scss/shidashi.scss";

let shidashi;

function ensureShidashi() {
  if(shidashi) { return(shidashi); }
  shidashi = new Shidashi();
  return(shidashi);
}

function getModuleId(item) {
  if(!item.length) {
    return;
  }
  const re = /^tab--module-(.*)-shared_id-[a-zA-Z0-9]+$/g;
  let module_id = re.exec(item[0].id);
  if(module_id && module_id.length > 0)  {
    module_id = module_id[1];
    return( module_id )
  } else {
    return;
  }
}

let initialized = false;
function initShidashi() {
  if(initialized) {
    return( shidashi );
  }
  initialized = true;
  ensureShidashi();
  const $iframeWrapper = $('.content-wrapper');

  const onTabChanged = (item) => {

    const module_id = getModuleId(item);
    if(!module_id) { return(item); }

    shidashi._active_module = module_id;
    const data = {
      type: "active_module",
      id : module_id,
      label : $(item[0]).text().trim()
    };
    shidashi.shinySetInput("@rave_action@", data, true, true);
    // shidashi.notifyIframes("shinySetInput", ["@rave_action@", data, true, true]);

    shidashi.removeClass("body", "scroller-not-top navbar-hidden");
    shidashi.notifyIframes("resumeStatus", [shidashi]);
    return item;
  }

  const adminLTEIframeHandler = $iframeWrapper.data('adminLTEIframeHandler');

  if( adminLTEIframeHandler ) {
    console.debug("AdminLTE: using existing IFrame handler");
    adminLTEIframeHandler._config.onTabChanged = onTabChanged;
  } else {
    console.debug("AdminLTE: creating new IFrame handler");
    $iframeWrapper.IFrame({
      onTabClick: (item) => {
        return item;
      },
      onTabChanged: onTabChanged,
      onTabCreated: (item) => {
        return item;
      },
      autoIframeMode: true,
      autoItemActive: true,
      autoShowNewTab: true,
      allowDuplicates: false,
      loadingScreen: false,
      useNavbarItems: false,
      scrollOffset: 0
    });
  }
  return( shidashi );
}



function registerShidashi(shiny) {
  ensureShidashi();
  initShidashi();
  shidashi._shiny = shiny;
  shidashi._register_shiny();
  shidashi.shiny_connected = true;
  shidashi.ensureShiny();
  shidashi._finalize_initialization();

  registerProgressOutput(shiny);
  registerClipboardOutput(shiny, shidashi);


  return( shidashi );
}

export { initShidashi, registerShidashi };





/*if (window.hljs) {
  hljs.configure({languages: []});
  hljs.initHighlightingOnLoad();
  if (document.readyState && document.readyState === "complete") {
    window.setTimeout(function() { hljs.initHighlighting(); }, 0);
  }
}*/

// $(document).ready
