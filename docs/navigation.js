window.addEventListener('DOMContentLoaded', () => {

    const activateAll = (element) => {
        element.classList.add('active');
        const ancestor = element.parentElement.parentElement;
        if (ancestor.localName === 'li') {
            activateAll(ancestor);
        }
    }

    const observer = new IntersectionObserver(entries => {
        entries.forEach(entry => {
            const id = entry.target.getAttribute('id');
            console.log(`scrolled to ${id}`);
            const element = document.querySelector(`nav li a[href="#${id}"]`);
            if (element) {
                const parent = element.parentElement;
                if (entry.intersectionRatio > 0) {
                    activateAll(parent);
                } else {
                    parent.classList.remove('active');
                }
            }
        });
    });

    // Track all sections that have an `id` applied
    document.querySelectorAll('section[id]').forEach((section) => {
        observer.observe(section);
    });
    // Track all components that have an `id` applied
    document.querySelectorAll(".component[id]").forEach((section) => {
        observer.observe(section);
    });

});
