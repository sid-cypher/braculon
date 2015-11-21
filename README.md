# braculon

## What is it?
Clack-based modular web framework with most flexible request routing.

**WARNING**: this is not even its final form.
Do not use for anything but throwaway projects.

My main goal for `braculon` is to provide all necessary infrastructure for your web app without getting in your way, and to make sure you have full and absolute control over your HTTP(S) request processing.

## Why would I want it?

- At the time of this writing, all (known to me) Clack-based frameworks use myway-based *URL+method routing* without giving you any convenient way to override default routing behavior. This `braculon` allows chaining *HTTP request routers* of your choice (both built-in and user-defined) in any order you like.
- Wrapping your app in (currently undocumented) opaque "*middlewares*" hurts readability, long-term consistency and modification ease. Extending the pipeline through *overriding* its steps and making use of *hooks* between these steps is a more flexible and readable way, and `braculon` makes it easy to do so.
- Avoid being forced into using any specific HTML/CSS/JS generator. With `braculon` and its *loadable view compilers* you can put together your final output from a multitude of *different representations* of your choice (Djula, CL-WHO, Markdown, QtDesigner UI forms, anything can be a view with the right compiler module)
- There will be *feature extension modules* as well, like a *system events monitor*, a Websocket/WebRTC *service manager*, runtime auto-updates for code definitions on *file change notifications* (e.g. a *git push*), and so on.

## Where is the documentation?

Sorry, haven't written it yet. Hate the lack of docs myself.
I'll definitely get to it as soon as the codebase stabilises a little bit.

## License

See the [LICENSE](LICENSE.md) file (MIT).
