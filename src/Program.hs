module Program where
import Data.Text

type Identifier = Text
type Progam = [Statement]

data Op = Plus | Minus | Divide | Multiply
data Comp = LE | GE | GT | LT | EQ | NEQ
data Expression = Number Int | FuncCall Identifier [Expression] | ArithmeticExpression Expression Op Expression | BooleanExpression Expression Comp Expression
data Statement = ProcCall Identifier [Expression] | Assignment Identifier Expression | FuncDefinition Identifier [Identifier] [Statement] | IfStatement Expression [Statement] [Statement] | While Expression [Statement]