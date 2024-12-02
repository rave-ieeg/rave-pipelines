$( document ).ready(function() {
    Shiny.addCustomMessageHandler('set_url', (msg) => {
        const url = new URL(`../${msg}`, window.location).href;
        const $a = document.createElement('a');
        $a.style.display = 'none';
        $a.href = url;
        document.body.appendChild($a);
        $a.click();
    })
});