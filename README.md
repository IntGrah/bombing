# Bombing

Client dependencies: `pnpm i`

Run client: `pnpm dev`

Expose to internet: `cloudflared tunnel --url localhost:5173` (install cloudflared)

Server dependencies: `opam install base stdio dream cohttp cohttp-lwt-unix`

Run server: `dune exec guandan`
