module Parskell.Synthesis.Letifier where

import Data.Text
import Parskell.ExpressionTree
import Parskell.Synthesis.Counter


varNameOfNumber :: Int -> Text
--varNameOfNumber countNum = Data.Text.pack ("#" ++ show countNum)
varNameOfNumber = Data.Text.pack . (++) "#" . show



letifyNesting :: Expression -> Counter Expression

letifyNesting Assignee { assigneeName = an } = return Assignee { assigneeName = an }
letifyNesting (Const c) = return (Const c)

letifyNesting Operation1 { unaryOperator = op, opExpression = Assignee { assigneeName = an }} = 
    return (Operation1 { unaryOperator = op, opExpression = Assignee { assigneeName = an }})
    
letifyNesting Operation1 { unaryOperator = op, opExpression = Const c } = 
    return (Operation1 { unaryOperator = op, opExpression = Const c})
    
letifyNesting Operation1 { unaryOperator = op, opExpression = expr } = do
    newVariableName <- varNameOfNumber <$> Parskell.Synthesis.Counter.retrieve
    innerExpr <- letifyNesting expr
    return LetExpression { 
        letAssignments = [
            Assignment { assignmentIdentifier = newVariableName, assignmentExpression = innerExpr }
        ],
        letExpression = Operation1 { 
            unaryOperator = op,  
            opExpression = Assignee { assigneeName = newVariableName }
        }
    }

letifyNesting Operation2 { binaryOperator = op, expression1 = Const c1, expression2 = Const c2 } =
    return Operation2 { binaryOperator = op, expression1 = Const c1, expression2 = Const c2 }
    
letifyNesting Operation2 { binaryOperator = op, expression1 = Const c, expression2 = Assignee { assigneeName = an } } =
    return Operation2 { binaryOperator = op, expression1 = Const c, expression2 = Assignee { assigneeName = an } }
    
letifyNesting Operation2 { binaryOperator = op, expression1 = Assignee { assigneeName = an }, expression2 = Const c } =
    return Operation2 { binaryOperator = op, expression1 = Assignee { assigneeName = an }, expression2 = Const c }
    
letifyNesting Operation2 { binaryOperator = op, expression1 = Assignee { assigneeName = an1 }, expression2 = Assignee { assigneeName = an2 } } =
    return Operation2 { binaryOperator = op, expression1 = Assignee { assigneeName = an1 }, expression2 = Assignee { assigneeName = an2 } }

letifyNesting Operation2 { binaryOperator = op, expression1 = expr, expression2 = Const c } = do
    newVariableName <- varNameOfNumber <$> Parskell.Synthesis.Counter.retrieve
    innerExpr <- letifyNesting expr
    return LetExpression { 
        letAssignments = [
            Assignment { assignmentIdentifier = newVariableName, assignmentExpression = innerExpr }
        ],
        letExpression = Operation2 { 
            binaryOperator = op,
            expression1 = Assignee { assigneeName = newVariableName },
            expression2 = Const c
        }
    }

letifyNesting Operation2 { binaryOperator = op, expression1 = expr, expression2 = Assignee { assigneeName = an } } = do
    newVariableName <- varNameOfNumber <$> Parskell.Synthesis.Counter.retrieve
    innerExpr <- letifyNesting expr
    return LetExpression { 
        letAssignments = [
            Assignment { assignmentIdentifier = newVariableName, assignmentExpression = innerExpr }
        ],
        letExpression = Operation2 { 
            binaryOperator = op,
            expression1 = Assignee { assigneeName = newVariableName },
            expression2 = Assignee { assigneeName = an }
        }
    }

letifyNesting Operation2 { binaryOperator = op, expression1 = Const c, expression2 = expr } = do
    newVariableName <- varNameOfNumber <$> Parskell.Synthesis.Counter.retrieve
    innerExpr <- letifyNesting expr
    return LetExpression { 
        letAssignments = [
            Assignment { assignmentIdentifier = newVariableName, assignmentExpression = innerExpr }
        ],
        letExpression = Operation2 { 
            binaryOperator = op,
            expression1 = Const c,
            expression2 = Assignee { assigneeName = newVariableName }
        }
    }

letifyNesting Operation2 { binaryOperator = op, expression1 = Assignee { assigneeName = an }, expression2 = expr } = do
    newVariableName <- varNameOfNumber <$> Parskell.Synthesis.Counter.retrieve
    innerExpr <- letifyNesting expr
    return LetExpression { 
        letAssignments = [
            Assignment { assignmentIdentifier = newVariableName, assignmentExpression = innerExpr }
        ],
        letExpression = Operation2 { 
            binaryOperator = op,
            expression1 = Assignee { assigneeName = an },
            expression2 = Assignee { assigneeName = newVariableName }
        }
    }

letifyNesting Operation2 { binaryOperator = op, expression1 = expr1, expression2 = expr2 } = do
    newVariableName1 <- varNameOfNumber <$> Parskell.Synthesis.Counter.retrieve
    newVariableName2 <- varNameOfNumber <$> Parskell.Synthesis.Counter.retrieve
    innerExpr1 <- letifyNesting expr1
    innerExpr2 <- letifyNesting expr2
    return LetExpression { 
        letAssignments = [
            Assignment { assignmentIdentifier = newVariableName1, assignmentExpression = innerExpr1 },
            Assignment { assignmentIdentifier = newVariableName2, assignmentExpression = innerExpr2 }
        ],
        letExpression = Operation2 { 
            binaryOperator = op,
            expression1 = Assignee { assigneeName = newVariableName1 },
            expression2 = Assignee { assigneeName = newVariableName2 }
        }
    }

letifyNesting LetExpression { letAssignments = la, letExpression = lexpr } = do
    innerExpr <- letifyNesting lexpr
    return LetExpression { 
        letAssignments = la,
        letExpression = innerExpr
    }

letifyNesting DoExpression { doStatements = ss, doExpression = dexpr } = do
    innerExpr <- letifyNesting dexpr
    return DoExpression { 
        doStatements = ss,
        doExpression = innerExpr
    }
    
