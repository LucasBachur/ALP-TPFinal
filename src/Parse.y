{
module Parse where
import Common
import Data.Maybe
import Data.Char

}

%monad { P } { thenP } { returnP }
%name parseStmt Comm
%name parseStmts Defs

%tokentype { Token }
%lexer {lexer} {TEOF}

%token
    '='      { TEquals }
    '+'      { TPlus }
    '-'      { TMinus }
    ':'      { TColon }
    ','      { TComma }
    '('      { TOpen }
    ')'      { TClose }
    '['      { TOpenC }
    ']'      { TCloseC }
    VAR      { TVar $$ }
    NAME     { TName $$ }   
    NUM      { TNum $$ }
    INC      { TInCommon }
    SHOW     { TShow }
    FILTER   { TFilter }
    ISDECK   { TIsDeck }
    MINION   { TMinion }
    SPELL    { TSpell }
    WEAPON   { TWeapon }
    HERO     { THero }
    LOCATION { TLocation }
    ATTACK   { TAttack }
    HEALTH   { THealth }
    COST     { TCost }
    TWOCOP   { T2Copies }
    IMPORTD  { TImport $$ }
    EXPORTD  { TExport }
    CARDDATA { TCardData }
    

%right VAR
%left '=' 
%left ',' '+' 

%%

Comm       :: { Comm }
           :  Def                                       { $1 }
           |  SHOW DeckExp                              { Show $2 }
           |  ISDECK DeckExp                            { IsDeck $2 }
           |  EXPORTD DeckExp DeckHero                  { Export $2 $3 }
           |  CARDDATA CardDef                          { GetCardData $2 }

DeckHero   :: { DeckHero }
           :  VAR                                       { Class $1 }
           |  NAME                                      { HeroName $1 }

Def        :  VAR '=' DeckExp                           { Def $1 $3 } 

DeckExp    :: { DeckExp }
           :  DeckExp '+' DeckExp                       { Union $1 $3 }
           |  DeckExp '-' DeckExp                       { Diff $1 $3 }
           |  INC '(' DeckExp DeckExp ')'               { InCommon $3 $4 }
           |  FILTER '(' '[' FilterList ']' DeckExp ')' { Filter $4 $6 }
           |  '[' CardList ']'                          { CardList $2 }
           |  VAR                                       { Deck $1 }
           |  IMPORTD                                   { Import $1 }
        
CardList   :: { CardList }
           :  CardUnit ',' CardList                     { $1 : $3 }
           |  CardUnit                                  { [$1] }
           |                                            { [] }

CardUnit   :: { CardUnit }
           :  CardDef                                   { One $1 }
           |  TWOCOP CardDef                            { Two $2 }

CardDef    :: { CardDef }
           :  NAME                                      { Name $1 }
           |  NUM                                       { Id $1 }

FilterList :: { [Filter] }
           :  Filter ',' FilterList                     { $1 : $3 }
           |  Filter                                    { [$1] }
           |                                            { [] }

Filter     :: { Filter }
           :  Field                                     { Field $1 }
           |  FieldN ':' NUM                            { FieldN $1 $3 }

Field      :: { Field }
           :  MINION                                    { Minion }
           |  SPELL                                     { Spell }
           |  WEAPON                                    { Weapon }
           |  HERO                                      { Hero }
           |  LOCATION                                  { Location }

FieldN     :: { FieldN }
           :  ATTACK                                    { Attack }
           |  HEALTH                                    { Health }
           |  COST                                      { Cost }

Defs       : Def Defs                                   { $1 : $2 }
           |                                            { [] }
     
{

data ParseResult a = Ok a | Failed String
                     deriving Show                     
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l-> case m s l of
                         Ok a     -> k a s l
                         Failed e -> Failed e
                         
returnP :: a -> P a
returnP a = \s l-> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l -> case m s l of
                        Ok a     -> Ok a
                        Failed e -> k e s l

happyError :: P a
happyError = \ s i -> Failed $ "Línea "++(show (i::LineNumber))++": Error de parseo\n"++(s)

data Token = TVar String
               | TName String
               | TNum Int
               | TImport String
               | TEquals
               | TPlus
               | TMinus
               | TColon
               | TComma
               | TOpen
               | TClose 
               | TOpenC
               | TCloseC
               | TInCommon
               | TShow
               | TFilter
               | TIsDeck
               | TMinion
               | TSpell
               | TWeapon
               | THero
               | TLocation
               | TAttack
               | THealth
               | TCost
               | T2Copies
               | TExport
               | TCardData
               | TEOF
               deriving Show

----------------------------------
lexer cont s = case s of
                    [] -> cont TEOF []
                    ('\n':s)  ->  \line -> lexer cont s (line + 1)
                    ('2':('x':cs)) -> cont T2Copies cs
                    (c:cs)
                          | isSpace c -> lexer cont cs
                          | isAlpha c -> lexVar (c:cs)
                          | isDigit c -> lexNum (c:cs)
                    ('"':cs) -> consumirName "" cont cs
                    ('+':cs) -> cont TPlus cs
                    ('-':cs) -> cont TMinus cs
                    (',':cs) -> cont TComma cs
                    ('(':cs) -> cont TOpen cs
                    (')':cs) -> cont TClose cs
                    ('[':cs) -> cont TOpenC cs
                    (']':cs) -> cont TCloseC cs
                    (':':cs) -> cont TColon cs
                    ('=':cs) -> cont TEquals cs
                    unknown -> \line -> Failed $ 
                     "Línea "++(show line)++": No se puede reconocer "++(show $ take 10 unknown)++ "..."
                    where lexVar cs = case span isAlphaNum cs of
                              ("inCommon",rest)     -> cont TInCommon rest
                              ("show",rest)         -> cont TShow rest
                              ("filter",rest)       -> cont TFilter rest
                              ("isDeck",rest)       -> cont TIsDeck rest
                              ("Minion",rest)       -> cont TMinion rest
                              ("Spell",rest)        -> cont TSpell rest
                              ("Weapon",rest)       -> cont TWeapon rest
                              ("Hero",rest)         -> cont THero rest
                              ("Location",rest)     -> cont TLocation rest
                              ("Attack",rest)       -> cont TAttack rest
                              ("Health",rest)       -> cont THealth rest
                              ("Cost",rest)         -> cont TCost rest
                              ("import",rest)       -> consumirCode rest
                              ("export",rest)       -> cont TExport rest
                              ("cardData",rest)     -> cont TCardData rest
                              (var,rest)            -> cont (TVar var) rest
                          lexNum cs = let (num,rest) = span isDigit cs 
                              in cont (TNum (read num)) rest
                          consumirName n cont s = case s of
                              ('"':cs) -> cont (TName n) cs
                              (c:cs) -> consumirName (n++[c]) cont (cs)
                          consumirCode cs = let (_,rest) = span (' '==) cs
                                                (code,rest') = span iscodeChar rest
                                                in cont (TImport code) rest'
                          iscodeChar c = (isAlphaNum c) || (c == '+') || (c == '/') || (c == '=')
                                  
stmts_parse s = parseStmts s 1
stmt_parse s = parseStmt s 1
}
