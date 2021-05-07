module Parskell.Synthesis.Letifier where

import Data.Text
import Parskell.ExpressionTree

letifyNesting :: Expression -> Expression

letifyNesting Assignee { assigneeName = an } = Assignee { assigneeName = an }
letifyNesting (Const c) = Const c

letifyNesting Operation1 { unaryOperator = op, opExpression = Assignee { assigneeName = an }} = 
    Operation1 { unaryOperator = op, opExpression = Assignee { assigneeName = an }}
    
letifyNesting Operation1 { unaryOperator = op, opExpression = Const c } = 
    Operation1 { unaryOperator = op, opExpression = Const c}
    
letifyNesting Operation1 { unaryOperator = op, opExpression = expr } = 
    LetExpression { 
        letAssignments = [
            Assignment { assignmentIdentifier = Data.Text.pack "#hugo", assignmentExpression = letifyNesting expr }
        ],
        letExpression = Operation1 { 
            unaryOperator = op,  
            opExpression = Assignee { assigneeName = Data.Text.pack "#hugo" }
        }
    }

letifyNesting Operation2 { binaryOperator = op, expression1 = Const c1, expression2 = Const c2 } =
    Operation2 { binaryOperator = op, expression1 = Const c1, expression2 = Const c2 }
    
letifyNesting Operation2 { binaryOperator = op, expression1 = Const c, expression2 = Assignee { assigneeName = an } } =
    Operation2 { binaryOperator = op, expression1 = Const c, expression2 = Assignee { assigneeName = an } }
    
letifyNesting Operation2 { binaryOperator = op, expression1 = Assignee { assigneeName = an }, expression2 = Const c } =
    Operation2 { binaryOperator = op, expression1 = Assignee { assigneeName = an }, expression2 = Const c }
    
letifyNesting Operation2 { binaryOperator = op, expression1 = Assignee { assigneeName = an1 }, expression2 = Assignee { assigneeName = an2 } } =
    Operation2 { binaryOperator = op, expression1 = Assignee { assigneeName = an1 }, expression2 = Assignee { assigneeName = an2 } }

letifyNesting Operation2 { binaryOperator = op, expression1 = expr, expression2 = Const c } =
    LetExpression { 
        letAssignments = [
            Assignment { assignmentIdentifier = Data.Text.pack "#hugo", assignmentExpression = letifyNesting expr }
        ],
        letExpression = Operation2 { 
            binaryOperator = op,
            expression1 = Assignee { assigneeName = Data.Text.pack "#hugo" },
            expression2 = Const c
        }
    }

letifyNesting Operation2 { binaryOperator = op, expression1 = expr, expression2 = Assignee { assigneeName = an } } =
    LetExpression { 
        letAssignments = [
            Assignment { assignmentIdentifier = Data.Text.pack "#hugo", assignmentExpression = letifyNesting expr }
        ],
        letExpression = Operation2 { 
            binaryOperator = op,
            expression1 = Assignee { assigneeName = Data.Text.pack "#hugo" },
            expression2 = Assignee { assigneeName = an }
        }
    }

letifyNesting Operation2 { binaryOperator = op, expression1 = Const c, expression2 = expr } =
    LetExpression { 
        letAssignments = [
            Assignment { assignmentIdentifier = Data.Text.pack "#hugo", assignmentExpression = letifyNesting expr }
        ],
        letExpression = Operation2 { 
            binaryOperator = op,
            expression1 = Const c,
            expression2 = Assignee { assigneeName = Data.Text.pack "#hugo" }
        }
    }

letifyNesting Operation2 { binaryOperator = op, expression1 = Assignee { assigneeName = an }, expression2 = expr } =
    LetExpression { 
        letAssignments = [
            Assignment { assignmentIdentifier = Data.Text.pack "#hugo", assignmentExpression = letifyNesting expr }
        ],
        letExpression = Operation2 { 
            binaryOperator = op,
            expression1 = Assignee { assigneeName = an },
            expression2 = Assignee { assigneeName = Data.Text.pack "#hugo" }
        }
    }

letifyNesting Operation2 { binaryOperator = op, expression1 = exp1, expression2 = exp2 } =
    LetExpression { 
        letAssignments = [
            Assignment { assignmentIdentifier = Data.Text.pack "#hugo1", assignmentExpression = letifyNesting exp1 },
            Assignment { assignmentIdentifier = Data.Text.pack "#hugo2", assignmentExpression = letifyNesting exp2 }
        ],
        letExpression = Operation2 { 
            binaryOperator = op,
            expression1 = Assignee { assigneeName = Data.Text.pack "#hugo1" },
            expression2 = Assignee { assigneeName = Data.Text.pack "#hugo2" }
        }
    }

letifyNesting LetExpression {letAssignments = la, letExpression = lexp} =
    LetExpression { 
        letAssignments = la,
        letExpression = letifyNesting lexp
    }

letifyNesting DoExpression {doStatements = ss, doExpression = dexp} =
    DoExpression { 
        doStatements = ss,
        doExpression = letifyNesting dexp
    }
    
