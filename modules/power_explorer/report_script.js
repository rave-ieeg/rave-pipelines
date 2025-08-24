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
  const toggle_buttons = document.querySelectorAll("[data-toggle='collapse']");
  toggle_buttons.forEach((btn) => {

    const collapse = btn.classList.contains("collapsed");
    const target_selector = btn.getAttribute("data-target");
    const targets = document.querySelectorAll(target_selector);

    btn.setAttribute("aria-expanded", collapse ? "false" : "true");
    targets.forEach(target => {
      if( collapse ) {
        target.classList.add("collapse");
        target.setAttribute("aria-expanded", "false");
        target.style.height = "0";
      } else {
        target.classList.remove("collapse");
        target.setAttribute("aria-expanded", "true");
        target.style.height = null;
      }
    });
  })
});
