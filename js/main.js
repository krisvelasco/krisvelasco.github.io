/* ============================================
   Kristopher Velasco — Site Interactions
   ============================================ */

document.addEventListener('DOMContentLoaded', () => {

  // --- Publication Filters ---
  const filterBtns = document.querySelectorAll('.filter-btn');
  const pubEntries = document.querySelectorAll('.pub-entry');
  const sectionTitles = document.querySelectorAll('.pub-section-title');

  filterBtns.forEach(btn => {
    btn.addEventListener('click', () => {
      const filter = btn.dataset.filter;

      // Update active button
      filterBtns.forEach(b => b.classList.remove('active'));
      btn.classList.add('active');

      // Filter entries
      pubEntries.forEach(entry => {
        const type = entry.dataset.type;
        if (filter === 'all' || type === filter) {
          entry.style.display = '';
        } else {
          entry.style.display = 'none';
        }
      });

      // Show/hide section titles
      sectionTitles.forEach(title => {
        const section = title.dataset.section;
        if (filter === 'all' || section === filter) {
          title.style.display = '';
        } else {
          title.style.display = 'none';
        }
      });
    });
  });

  // --- Fade-in animations with staggered delay ---
  const animateElements = document.querySelectorAll('.animate-in');

  animateElements.forEach((el, i) => {
    const delay = Math.min(i * 40, 600);
    setTimeout(() => el.classList.add('visible'), delay);
  });

  // --- Mobile nav close on link click ---
  const navLinks = document.getElementById('navLinks');
  if (navLinks) {
    navLinks.querySelectorAll('a').forEach(link => {
      link.addEventListener('click', () => {
        navLinks.classList.remove('open');
      });
    });
  }

});
