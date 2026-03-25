/* ============================================
   Kristopher Velasco — Site Interactions
   ============================================ */

document.addEventListener('DOMContentLoaded', () => {

  // --- State for dual filtering ---
  let activeType = 'all';
  let activeTheme = 'all';

  const pubEntries = document.querySelectorAll('.pub-entry');
  const sectionTitles = document.querySelectorAll('.pub-section-title');

  function applyFilters() {
    pubEntries.forEach(entry => {
      const type = entry.dataset.type || '';
      const themes = (entry.dataset.theme || '').split(' ');

      const matchType = activeType === 'all' || type === activeType;
      const matchTheme = activeTheme === 'all' || themes.includes(activeTheme);

      entry.style.display = (matchType && matchTheme) ? '' : 'none';
    });

    // Show/hide section titles + their pub-lists based on visible entries
    sectionTitles.forEach(title => {
      const list = title.nextElementSibling;
      if (list) {
        const visibleEntries = list.querySelectorAll('.pub-entry:not([style*="display: none"])');
        const hasVisible = visibleEntries.length > 0;
        title.style.display = hasVisible ? '' : 'none';
        list.style.display = hasVisible ? '' : 'none';
      }
    });
  }

  // --- Type filter buttons ---
  const typeFilterBtns = document.querySelectorAll('#typeFilters .filter-btn');
  typeFilterBtns.forEach(btn => {
    btn.addEventListener('click', () => {
      activeType = btn.dataset.filter;
      typeFilterBtns.forEach(b => b.classList.remove('active'));
      btn.classList.add('active');
      applyFilters();
    });
  });

  // --- Theme filter buttons ---
  const themeFilterBtns = document.querySelectorAll('#themeFilters .filter-btn');
  themeFilterBtns.forEach(btn => {
    btn.addEventListener('click', () => {
      activeTheme = btn.dataset.themeFilter;
      themeFilterBtns.forEach(b => b.classList.remove('active'));
      btn.classList.add('active');
      applyFilters();
    });
  });

  // --- Handle URL hash for theme filtering (from nav dropdown) ---
  function checkHash() {
    const hash = window.location.hash.replace('#', '');
    if (hash && ['world-culture', 'nonprofits', 'queer-demography'].includes(hash)) {
      activeTheme = hash;
      themeFilterBtns.forEach(b => {
        b.classList.toggle('active', b.dataset.themeFilter === hash);
      });
      applyFilters();
    }
  }
  checkHash();
  window.addEventListener('hashchange', checkHash);

  // --- Abstract toggles ---
  document.querySelectorAll('.abstract-toggle').forEach(btn => {
    btn.addEventListener('click', () => {
      const content = btn.nextElementSibling;
      const isOpen = content.classList.toggle('open');
      btn.innerHTML = isOpen ? 'Abstract &#9652;' : 'Abstract &#9662;';
    });
  });

  // --- Fade-in animations with staggered delay ---
  const animateElements = document.querySelectorAll('.animate-in');
  animateElements.forEach((el, i) => {
    const delay = Math.min(i * 40, 600);
    setTimeout(() => el.classList.add('visible'), delay);
  });

  // --- Mobile nav ---
  const navLinks = document.getElementById('navLinks');
  if (navLinks) {
    navLinks.querySelectorAll('a').forEach(link => {
      link.addEventListener('click', () => {
        navLinks.classList.remove('open');
      });
    });
  }

  // --- Nav dropdown toggle (for mobile) ---
  document.querySelectorAll('.nav-dropdown > a').forEach(link => {
    link.addEventListener('click', (e) => {
      if (window.innerWidth <= 768) {
        e.preventDefault();
        link.parentElement.classList.toggle('open');
      }
    });
  });

});
