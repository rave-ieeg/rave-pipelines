document.addEventListener("DOMContentLoaded", function () {
  // For custom TOC
  let count = 0;
  const insert_custom_toc = () => {
    const custom_toc = document.getElementById("custom-toc");
    const toc = document.querySelector("d-article.d-article-with-toc #TOC");
    if(!custom_toc) { return; }
    if(toc) {
      toc.appendChild(custom_toc);
      return;
    }
    if(count > 10) { return; }
    count++;
    console.log(toc);
    setTimeout(insert_custom_toc, 500);
  };
  insert_custom_toc();
});

document.addEventListener("DOMContentLoaded", function () {
  // for toggle button
  const toggle_buttons = document.querySelectorAll("[data-toggle='collapse']")
  toggle_buttons.forEach((btn) => {
    btn.onclick = function() {
      const target_selector = this.getAttribute("data-target");
      const targets = document.querySelectorAll(target_selector);
      targets.forEach(target => {
        if ( target.style.display == 'block' || target.style.display == '' ) {
          target.style.display = 'none';
        } else {
          target.style.display = 'block';
        }
      })
    }
  })
});
