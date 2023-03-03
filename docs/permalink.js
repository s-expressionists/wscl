window.addEventListener('DOMContentLoaded', () => {

    const makeURL = (newId) => {
        const url = new window.URL(document.location);
        url.hash = newId;
        return url.href;
    }

    document.querySelectorAll('section[id]').forEach((section) => {
        const id = section.getAttribute('id');
        const url = makeURL(id)
        const heading = section.querySelector('.section-title');
        heading.innerHTML += ` <a class="permalink" href="${url}">¶</a>`;
    });

    document.querySelectorAll(".component .name[id]").forEach((name) => {
        const id = name.getAttribute('id');
        const url = makeURL(id);
        name.innerHTML += ` <a class="permalink" href="${url}">¶</a>`;
    });

});
