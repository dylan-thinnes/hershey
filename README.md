Paul Bourke helpfully encoded the [Hershey font](https://en.wikipedia.org/wiki/Hershey_fonts) in a pretty simple format at [https://paulbourke.net/dataformats/hershey/](https://paulbourke.net/dataformats/hershey/)

I have written a translator which can accept this format and converts each glyph to a Diagram in the Haskell `diagrams` library.

This is helpful for my slide rule laser cutting / laser engraving projects, but can hopefully also be useful to other people who need a simple vector font in Haskell.
