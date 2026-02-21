# FluxPay — Frontend (React + Vite)

This repository contains the FluxPay frontend built with React, Vite, Tailwind CSS and React Router.

Quick overview
- Main entry: `src/main.jsx`
- App component & routes: `src/App.jsx`
- Components: `src/components/`
- Pages: `src/pages/`
- Simple blog demo data: `src/lib/blogData.js`

Getting started

1. Install dependencies

```bash
npm install
```

2. Run development server

```bash
npm run dev
```

3. Open in browser

Visit `http://localhost:5173` (Vite default) or the URL printed by the dev server.

Build for production

```bash
npm run build
npm run preview
```

Routing notes
- The app uses `react-router-dom`. The navbar links use `NavLink` components and pages live in `src/pages`.
- Blog list and read pages are available at `/blog` and `/blog/:slug`.

Editing
- Components live under `src/components/` (e.g., `Navbar.jsx`, `Footer.jsx`, `BlogCard.jsx`).
- Styles use Tailwind classes in `index.css` and component classes.

Troubleshooting
- If navigation doesn't work, ensure the app is wrapped with `<BrowserRouter>` in `src/main.jsx`.
- If the dev server fails to start, check the terminal output and share the error.

Want something else?
- I can add unit tests, a full blog list page, or wire up a CMS. Tell me which feature you want next.

Original UI source
- The original UI source for this project is `fampay`.
