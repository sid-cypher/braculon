# braculon

A modular web framework on top of Clack. Full featured not yet, but eventually.

WARNING: this is not even its final form.
Do not use for anything but throwaway projects.

Main goal of the project is to provide all necessary infrastructure for your web app
 without getting in your way, and to make sure you have full and absolute control
 over your HTTP(S) request processing.

## Why braculon?

- At the time of this writing, all (known to me) Clack-based frameworks use myway-based URL routing without giving you any convenient way to override default routing behavior.
- Wrapping your app in an (currently undocumented) opaque "middlewares" hurts consistency, readability and modification ease.
- Naming things is hard. Why not "braculon"?

## License

See the [LICENSE](LICENSE.md) file (MIT).