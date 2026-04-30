// Configuration générale du document
#set document()

// Configuration de la page avec en-têtes et pieds de page
#set page(
footer: align(center)[
  #set text(9pt, fill: gray.darken(20%))
],
header-ascent: 30%,
footer-descent: 30%,
)

// Style des titres
#set heading(numbering: "1.1")
#show heading: set block(above: 1.4em, below: 1em)

#show heading.where(level: 1): heading => {
set text(16pt, weight: "bold")
v(0.5em)
heading
v(0.3em)
}

#show heading.where(level: 2): set text(14pt, weight: "bold")
#show heading.where(level: 3): set text(12pt, weight: "bold")
#show heading.where(level: 4): set text(11pt, weight: "bold", style: "italic")

// Style des paragraphes
#set par(
justify: true,
leading: 0.65em,
first-line-indent: 1em,
)

// Ne pas indenter après les titres
#show heading: it => {
it
par(first-line-indent: 0pt)[#text(size:0pt)[#h(0pt)]]
}

// Style des listes
#set list(indent: 1em)
#set enum(indent: 1em)

// Style des tableaux
#set table(
fill: (x, y) => if y == 0 { gray.lighten(90%) },
stroke: 0.5pt + gray,
inset: 8pt,
)

#show table.cell.where(y: 0): set text(weight: "bold")

// Style des figures
#show figure: set block(breakable: false)
#show figure.caption: set text(size: 10pt)

// Style des équations
#show math.equation: set text(font: "New Computer Modern Math")

// Style des blocs de code
#set raw(theme: "_extensions/hebstr-doc/code.tmTheme")

#show raw.where(block: true): block.with(
  fill: rgb("#291334"),
  inset: 10pt,
  radius: 5pt,
  width: 100%,
)

// Style des liens
#show link: it => text(fill: blue.darken(20%), it)

// Style pour les citations
#show quote: set block(
inset: (left: 1.5em, right: 1.5em),
stroke: (left: 2pt + gray),
)
