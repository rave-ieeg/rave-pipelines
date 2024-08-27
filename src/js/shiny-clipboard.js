import $ from 'jquery';
import ClipboardJS from 'clipboard';

function registerClipboardOutput(Shiny, shidashi) {
  const clipboardOutputBinding = new Shiny.OutputBinding();
  const clsName = "shidashi-clipboard-output";
  clipboardOutputBinding.name = "shidashi.clipboardOutputBinding";

  $.extend(clipboardOutputBinding, {
    find: function(scope) {
      const $scope = $(scope);
      const re = [];

      $scope.each((i, el) => {
        const $el = $(el);
        if( $el.hasClass( clsName ) ) {
          re.push(el);
        } else {
          $el.find( `.${ clsName }` ).each( re.push );
        }
      })

      return $(re);
    },
    renderValue: function(el, value) {
      let el_ = $(el);
      if(!el_.hasClass("clipboard-btn")){
        el_ = $(el).find(".clipboard-btn");
      }
      $(el_).attr("data-clipboard-text", value);
    },
    renderError: function(el, err) {
      let el_ = $(el);
      if(!el_.hasClass("clipboard-btn")){
        el_ = $(el).find(".clipboard-btn");
      }
      $(el_).attr("data-clipboard-text", "Error: " + err.message);
    }
  });

  Shiny.outputBindings.register(clipboardOutputBinding, "shidashi.clipboardOutputBinding");

  Shiny.bindAll(".shidashi-clipboard-output");

  // No need to re-register because they use delegation
  new ClipboardJS(".clipboard-btn").on('success', function(e) {
    shidashi.createNotification({
      title : "Copied to clipboard",
      delay: 1000,
      autohide: true,
      icon: "fa fas fa-copy",
      "class" : "bg-success",
      "body"  : e.text
    });
    e.clearSelection();
  });
}

export { registerClipboardOutput }
