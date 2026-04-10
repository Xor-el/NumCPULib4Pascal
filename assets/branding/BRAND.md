# NumCPULib4Pascal — lightweight brand guide

## Primary mark

- **Default:** [`logo.svg`](logo.svg) — **SMT pairs**: a **2×2** grid of **physical core** disks (**gold** stroke) each with **paired cyan ticks** suggesting **two hardware threads per core**. Evokes **logical vs physical** CPU counts without implying a fixed core number for every machine.
- **Dark UI:** [`logo-dark.svg`](logo-dark.svg) — same layout with **amber** rings, **ice** thread ticks, **slate** core fills on a **near-black** blue badge.

## Palette (default logo)

| Role | Hex | Notes |
|------|-----|--------|
| Badge top | `#1d4ed8` | Blue gradient start. |
| Badge bottom | `#0f172a` | Slate indigo end. |
| Core fill | `#1e293b` | Disk interior. |
| Core stroke | `#fbbf24` | Physical core ring. |
| Thread ticks | `#22d3ee` | Logical thread pairs. |

Dark variant uses `#172554`–`#020617`, core fill `#334155`, stroke `#fde68a`, ticks `#67e8f9`.

**Banner background** (flat fill behind the logo for wide social and Open Graph PNGs [here](export/)): RGB **22, 51, 129** (`#163381`), midpoint between badge top and bottom.

## Typography (pairing)

The logo has **no embedded wordmark**. When setting type next to the mark:

- Prefer **clean sans-serif** UI fonts (e.g. Segoe UI, Inter, Source Sans 3).
- **Do not** use Embarcadero product logotypes alongside this mark in a way that suggests an official bundle.

## Clear space

Keep padding around the badge at least **1/4 of the mark width** on a square canvas. Do not crop flush against the rounded corners.

## Minimum size

- **Favicon / IDE:** target **16×16** in ICO; **32×32** or larger is clearer.
- **README / docs:** **128–200 px** wide for the SVG is typical.

## Correct use

- Scale **uniformly**.
- Use [`logo-dark.svg`](logo-dark.svg) on **dark** pages for contrast.
- Prefer **SVG** on the web; **PNG** where required (some social crawlers).

## Incorrect use

- Do not **stretch**, **skew**, or **rotate** the mark for effect.
- Do not **recolor** outside the documented palette without updating this doc (palette table above).
- Do not **crop** away the badge frame or drop the thread pairs (loses the SMT story).
- Do not place **third-party logos inside** the badge.

## Wordmark

“NumCPULib4Pascal” in plain text beside or below the mark is enough; no custom logotype is required.
