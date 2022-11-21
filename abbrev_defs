;;-*-coding: utf-8;-*-
(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ("#e" "#endif /* */" my-abbrev-hook :count 0)
    ("#i" "#include \"\"" my-abbrev-hook :count 0)
    ("#ifd" "#ifdef" nil :count 0)
    ("#ifn" "#ifndef" nil :count 0)
    ("#s" "#include <>" my-abbrev-hook :count 0)
    ("else" "else {
}
" my-abbrev-hook :count 0)
    ("for" "for (;;) {
}
" my-abbrev-hook :count 0)
    ("if" "if () {
}
" my-abbrev-hook :count 0)
    ("imain" "int
main (int ac, char **av[])
{

}" my-abbrev-hook :count 0)
    ("pr" "printf (\"\")" my-abbrev-hook :count 0)
    ("while" "while () {
}
" my-abbrev-hook :count 0)
   ))

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ("#e" "#endif /* */" my-abbrev-hook :count 0)
    ("#i" "#include \"\"" my-abbrev-hook :count 0)
    ("#ifd" "#ifdef" nil :count 0)
    ("#ifn" "#ifndef" nil :count 0)
    ("#s" "#include <>" my-abbrev-hook :count 0)
    ("else" "else {
}
" my-abbrev-hook :count 0)
    ("for" "for (;;) {
}
" my-abbrev-hook :count 0)
    ("if" "if () {
}
" my-abbrev-hook :count 1)
    ("imain" "int
main (int ac, char **av[])
{

}" my-abbrev-hook :count 0)
    ("pr" "printf (\"\")" my-abbrev-hook :count 0)
    ("while" "while () {
}
" my-abbrev-hook :count 0)
   ))

