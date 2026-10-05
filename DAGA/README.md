# REINASLEO — Monorepo

Premium womenswear homepage draft with Next.js (App Router) + Tailwind + next-intl and Spring Boot 3 API.

## Structure
- `apps/web` — Next.js 15, Tailwind, next-intl with `/[locale]` routes.
- `apps/api` — Spring Boot 3 (Java 21) minimal REST API with validation and gzip.

## Running locally
1) Web
```bash
cd apps/web
npm install
npm run dev
# open http://localhost:3000/en (locale prefix required; RU at /ru)
```
Set `NEXT_PUBLIC_API_BASE` if the API runs on a different port/host.

2) API (requires Gradle 8+/Java 21)
```bash
cd apps/api
gradle bootRun
# health: http://localhost:8080/api/health
```
Ports can be changed in `apps/api/src/main/resources/application.yml`.

## Localization
- Dictionaries live in `apps/web/messages/en.json` and `apps/web/messages/ru.json`.
- Locale routes are prefixed (`/en`, `/ru`) via next-intl middleware. Header toggle swaps locales on the current path.

## Brand assets
- Replace the placeholder emblem SVG inside `apps/web/components/BrandEmblem.tsx` (marked comment). Keep the `viewBox 0 0 64 64` for easiest drop-in.
- Swap hero/lookbook placeholders by replacing the framed divs in `app/[locale]/page.tsx` with your images/components.

## Styling knobs
- Background + texture intensity: `--texture-opacity` and `--crumple-opacity` in `apps/web/app/globals.css`.
- Accent color: Tailwind theme `accent` (`tailwind.config.ts`).
- Typography: display font `Bebas Neue`, body `Inter` defined in `app/layout.tsx`.

## API endpoints
- `GET /api/health` — uptime probe.
- `POST /api/contact` — accepts `{ name, email, message }`, validates, stores in-memory, returns id/timestamp.
- `GET /api/lookbook` — stub placeholders for future integration.

## Notes
- Header, footer, and content are server components by default; only the locale switcher and contact form use client interactivity.
- CORS allows `http://localhost:3000` by default; edit `app.cors.allowed-origins` in `application.yml`.
