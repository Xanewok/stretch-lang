# Stretch

Opis konkretnego języka z planowanymi funkcjonalnościami i gramatyką wysyłany
był wcześniej, dlatego zakładam, że nie jest to miejsce na dokładny opis tutaj,
w ramach wstępnej wersji interpretera.

Skrótowo, język Stretch jest językiem imperatywnym, którego składnia
zainspirowana jest językiem Rust.

# Rozwiązanie
## Frontend
Rozwiązanie wykorzystuje polecany generator frontendu BNFC. Definicja gramatyki
znajduje się w katalogu syntax/Stretch.cf, razem z pomocniczym skryptem
`generate-parser`, który odpowiada za wygenerowanie odpowiednich plików .hs i
skopiowanie ich do katalogi src/BNFC, do użycia przez właściwy interpreter.

# Interpreter
Interpreter operuje na wygenerowanym przez BNFC abstrakcyjnym drzewie, którego
definicja znajduje się w src/BNFC/AbsStretch.hs. Cały interpreter obecnie
znajduje się w pliku src/Lib.hs. Plik wykonywalny wołający najpierw parser/lexer
i potem interpreter, znajduje się w app/Main.hs.

Głównie zdefiniowane są funkcję eval* (np. evalExp, evalStm...), które operują
na głównym stosie monadycznym, `StateT MyEnv IO a`.
Typ `MyEnv` obecnie składa się z typów `(Env, Store, RecordFields)` (z czego
`RecordFields` jest tymczasowe). Type `Env` i `Store` są zdefiniowane
analogicznie jak na przedmiocie Semantyka i weryfikacja programów - jedno jest
mapą z identyfikatora w pewne położenie, natomaist drugie mapuje z tego
położenia w wartości typu algebraicznego `Value`.

Błędy obecnie są propagowane funkcją `fail` poprzez monadę `IO`.

# Niejednoznaczność gramatyki

Są obecnie 2 niejedoznaczności:
- shift/reduce - Wyrażenie `MyStruct { field1: 5 }` i `myStruct` oba zaczynają
  się od identyfikatora, natomiast pierwszy oznacza strukturę, a drugi
  identyfikator pewnej wartości. Tu, w przypadku gdy po identyfikatorze
  następuje `{` zawsze chodzi nam o wyrażenie struktury i parser robi zawsze to,
  co chcemy.
- reduce/reduce - Te wydają się poważne, natomiast jest to związane z projektem
  języka. Tu wszystko stara się być wyrażeniem i tak np. ostatnie wyrażenie w
  bloku jest wartością danego wyrażenia, np. `{ let a = 5; 3 }` jest wyrażeniem
  o wartości `3`. Kłóci się to gramatycznie z kontrolą przepływu programu, np.
  chcemy móc pisać po sobie instrukcje `if true {}`, które w końcu także są
  wyrażeniami. W związku z tym, gramatyka bloku `{ [Stm] Exp }` może napotkać na
  niejednoznaczność. Tutaj naszą preferencją jest mocniejsze wiązanie "w
  wyrażenie" - wyrażenie typu `{ if true { 1 } else { 0 } - 2 }` będzie
  interpretowane jako suma dwóch mniejszych wyrażeń, a nie `if`a, po którym
  zwracamy z bloku wartość `-2`.

# Czego nie ma
Na obecną chwilę nie ma:
- statycznego systemu typów
- nie wykrywanie złych typów funkcji
- nie są przekazywane argumenty do funkcji w trakcie jej wywołania
- statycznych bindingów

# Budowanie i uruchamianie projektu

Project był stworzony przy wykorzystaniu narzędzia `stack` i budowany oraz
testowany przy jego użyciu, dlatego preferowane jest to narzędzie. Projekt można
budować przy użyciu `stack build` oraz `stack exec stretchi nazwa_pliku`, gdzie
`nazwa_pliku` wskazuje do pliku z kodem źródłowym języka Stretch.

Narzędzie to generuje także pliki do wykorzystania przez narzędzie `cabal` - tu
analogicznie powinno działać polecenie `cabal build` celem zbudowania i `cabal
run nazwa_pliku` celem uruchomienia interpretera.