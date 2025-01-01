import ASTNodeDefs as AST
class Lexer:
    def __init__(self, code):
        self.code = code
        self.position = 0
        self.current_char = self.code[self.position]
        self.tokens = []
    
    # Move to the next position in the code increment by one.
    def advance(self):
        self.position += 1
        if self.position >= len(self.code):
            self.current_char = None
        else:
            self.current_char = self.code[self.position]

    # If the current char is whitespace, move ahead.
    def skip_whitespace(self):
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    # Tokenize the identifier.
    def identifier(self):
        result = ''
        while self.current_char is not None and (self.current_char.isalnum() or self.current_char == '_'):
            result += self.current_char
            self.advance()
        return ('IDENTIFIER', result)
    

    # Tokenize numbers, including float handling

    def number(self):
        """
        Handles both integer and floating-point numbers.
        """
        result = ''
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        if self.current_char == '.':
            result += self.current_char
            self.advance()
            while self.current_char is not None and self.current_char.isdigit():
                result += self.current_char
                self.advance()
            return ('FNUMBER', float(result))

        return ('NUMBER', int(result))

    def token(self):
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue
            if self.current_char.isalpha():
                ident = self.identifier()
                if ident[1] == 'if':
                    return ('IF', 'if')
                elif ident[1] == 'else':
                    return ('ELSE', 'else')
                elif ident[1] == 'while':
                    return ('WHILE', 'while')
                elif ident[1] == 'int':
                    return ('INT', 'int')
                elif ident[1] == 'float':
                    return ('FLOAT', 'float')
                return ident  # Generic identifier
            if self.current_char.isdigit() or self.current_char == '.':
                return self.number()
            if self.current_char == '+':
                self.advance()
                return ('PLUS', '+')
            if self.current_char == '-':
                self.advance()
                return ('MINUS', '-')
            if self.current_char == '*':
                self.advance()
                return ('MULTIPLY', '*')
            if self.current_char == '/':
                self.advance()
                return ('DIVIDE', '/')
            if self.current_char == '=':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return ('EQ', '==')
                return ('EQUALS', '=')
            if self.current_char == '!':
                self.advance()
                if self.current_char == '=':
                    self.advance()
                    return ('NEQ', '!=')
            if self.current_char == '<':
                self.advance()
                return ('LESS', '<')
            if self.current_char == '>':
                self.advance()
                return ('GREATER', '>')
            if self.current_char == '(':
                self.advance()
                return ('LPAREN', '(')
            if self.current_char == ')':
                self.advance()
                return ('RPAREN', ')')
            if self.current_char == ',':
                self.advance()
                return ('COMMA', ',')
            if self.current_char == ':':
                self.advance()
                return ('COLON', ':')
            if self.current_char == '{':
                self.advance()
                return ('LBRACE', '{')
            if self.current_char == '}':
                self.advance()
                return ('RBRACE', '}')
            # TODO: Implement handling for '{' and '}' tokens here (see `tokens.txt` for exact names)
            if self.current_char == '\n':
                self.advance()
                continue

            raise ValueError(f"Illegal character at position {self.position}: {self.current_char}")

        return ('EOF', None)

    # Collect all the tokens in a list.
    def tokenize(self):
        while True:
            token = self.token()
            self.tokens.append(token)
            if token[0] == 'EOF':
                break
        return self.tokens



import ASTNodeDefs as AST

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.current_token = tokens.pop(0)
        # Use these to track the variables and their scope
        self.symbol_table = {'global': {}}
        self.scope_counter = 0
        self.scope_stack = ['global']
        self.messages = []

    def error(self, message):
        self.messages.append(message)
    
    def advance(self):
        if self.current_token[0] == 'LBRACE':
            self.scope_counter += 1
            new_scope = f'scope_{self.scope_counter}'
            self.symbol_table[new_scope] = {}  
            self.scope_stack.append(new_scope) 
        elif self.current_token[0] == 'RBRACE':
            if len(self.scope_stack) > 1: 
                self.scope_stack.pop()  

        if self.tokens:  
            self.current_token = self.tokens.pop(0)

    # TODO: Implement logic to enter a new scope, add it to symbol table, and update `scope_stack`
    def enter_scope(self):
        self.scope_counter += 1
        new_scope = f'scope_{self.scope_counter}'
        self.symbol_table[new_scope] = {}  
        self.scope_stack.append(new_scope)  


    # TODO: Implement logic to exit the current scope, removing it from `scope_stack`
    def exit_scope(self):
        if self.scope_stack[-1] != 'global':  
            exited_scope = self.scope_stack.pop()  
            del self.symbol_table[exited_scope]  
         

    # Return the current scope name
    def current_scope(self):
        return self.scope_stack[-1]

    # TODO: Check if a variable is already declared in the current scope; if so, log an error
    def checkVarDeclared(self, identifier):
        curr_scope = self.current_scope()  
        if identifier in self.symbol_table[curr_scope]:  
            self.error(f"Variable {identifier} has already been declared in the current scope")
       

    # TODO: Check if a variable is declared in any accessible scope; if not, log an error
    def checkVarUse(self, identifier):
        is_declared = any(identifier in self.symbol_table[scope]
                        for scope in reversed(self.scope_stack))

        if not is_declared:
            
            self.error(f"Variable {identifier} has not been declared in the current or any enclosing scopes")

    # TODO: Check type mismatch between two entities; log an error if they do not match
    def checkTypeMatch2(self, vType, eType, var, exp):
        """Check type mismatch between two entities; log an error if they do not match."""

       
        if vType and eType and vType != eType:  
            
            error_message = f"Type Mismatch between {vType} and {eType}" 
            
           
            if error_message not in self.messages: 
                self.messages.append(error_message)

    # TODO: Implement logic to add a variable to the current scope in `symbol_table`
    def add_variable(self, name, var_type):
        """Add a variable to the current scope in `symbol_table`."""
        scope = self.current_scope() 

        
        if name in self.symbol_table.get(scope, {}):
            
            self.error(f"Variable '{name}' already declared in current scope")  
        
        
        self.symbol_table.setdefault(scope, {})[name] = var_type

    # TODO: Retrieve the variable type from `symbol_table` if it exists
    def get_variable_type(self, name):
        """Retrieve the variable type from `symbol_table` if it exists."""

        
        for scope in reversed(self.scope_stack):
            
            if name in self.symbol_table.get(scope, {}):
                return self.symbol_table[scope].get(name)  
        return None  # Variable not found in any scope
    
    def parse(self):
        return self.program()

    def program(self):
        statements = []
        while self.current_token[0] != 'EOF':
            statements.append(self.statement())
        return AST.Block(statements)

    # TODO: Modify the `statement` function to dispatch to declare statement
    def statement(self):
        if self.current_token[0] == 'IDENTIFIER':
            if self.peek() == 'EQUALS':
                return self.assign_stmt()
            elif self.peek() == 'LPAREN':
                return self.function_call()
            else:
                raise ValueError(f"Unexpected token after identifier: {self.current_token}")
        elif self.current_token[0] in ['INT', 'FLOAT']:  
            return self.decl_stmt()
        elif self.current_token[0] == 'IF':
            return self.if_stmt()
        elif self.current_token[0] == 'WHILE':
            return self.while_stmt()
        else:
            raise ValueError(f"Unexpected token: {self.current_token}")

    # TODO: Implement the declaration statement and handle adding the variable to the symbol table
    def decl_stmt(self):
        """
        Parses a declaration statement.
        Example:
        int x = 5
        float y = 3.5
        TODO: Implement logic to parse type, identifier, and initialization expression and also handle type checking
        """
        
        type_token = self.current_token
        var_type = type_token[1] 
        self.advance()  

        identifier_token = self.current_token
        if identifier_token[0] != 'IDENTIFIER':
            raise ValueError(f"Expected an identifier, found: {identifier_token}")
        var_name = identifier_token[1]  
        self.advance() 

        
        self.checkVarDeclared(var_name) if var_name in self.symbol_table[self.current_scope()] else self.symbol_table[self.current_scope()].update({var_name: var_type})

        expression = None  # Initialize expression to None in case there's no initialization
        
        # Handle initialization if present
        if self.current_token[0] == 'EQUALS':  
            self.advance() 
            expression = self.expression()  

            # Type checking 
            expr_type = self.get_expression_type(expression) 
            self.checkTypeMatch2(var_type, expr_type, var_name, expression) if expr_type != var_type else None

        
        return AST.Declaration(var_type, var_name, expression)

    # TODO: Parse assignment statements, handle type checking
    def assign_stmt(self):
        """
        Parses an assignment statement.
        Example:
        a = 10;
        """
        if self.current_token[0] != 'IDENTIFIER':
            raise ValueError(f"Expected an identifier, found: {self.current_token}")

        var_name = self.current_token[1]
        self.advance()

        var_type = self.get_variable_type(var_name)
        if var_type is None:
            self.checkVarUse(var_name)

        if self.current_token[0] != 'EQUALS':
            raise ValueError(f"Expected '=', found: {self.current_token}")
        self.advance()

        expression = self.expression()        
        
        if var_type is not None:
            expr_type = self.get_expression_type(expression)
            if expr_type is not None:                
                self.checkTypeMatch2(var_type, expr_type, var_name, expression)        

        return AST.Assignment(var_name, expression)

    def get_expression_type(self, expression):
        """
        Determines the type of a full expression. Avoid redundant type checking.
        """
        if isinstance(expression, AST.BinaryOperation):
            left_type = self.get_expression_type(expression.left)
            right_type = self.get_expression_type(expression.right)
            if left_type != right_type:
                self.checkTypeMatch2(left_type, right_type, expression.left, expression.right)
            return left_type  
        elif isinstance(expression, AST.Factor):
            return expression.value_type  
        else:
            raise ValueError(f"Unsupported expression type: {expression}")

    # TODO: Implement the logic to parse the if condition and blocks of code
    def if_stmt(self):
        """
        Parses an if-statement, with an optional else block.
        Example:
        if condition {
            # statements
        }
        else {
            # statements
        }
        TODO: Implement the logic to parse the if condition and blocks of code.
        """
        self._expect_token('IF')
        self.advance()

        condition = self.boolean_expression()

        self._expect_token('LBRACE')
        self.advance()

        then_block = self._parse_block()

        else_block = None
        if self.current_token[0] == 'ELSE':
            self.advance()
            self._expect_token('LBRACE')
            self.advance()
            else_block = self._parse_block()

        return AST.IfStatement(condition, then_block, else_block)


    def _expect_token(self, expected_type):
        """Helper function to check if the current token is of the expected type."""
        if self.current_token[0] != expected_type:
            raise ValueError(f"Expected '{expected_type}', found: {self.current_token}")

    def _parse_block(self):
        """Helper function to parse a block of statements enclosed in {}."""
        statements = []
        while self.current_token[0] != 'RBRACE':
            statements.append(self.statement())
            if self.current_token[0] == 'EOF':
                raise ValueError("Unexpected EOF while parsing a block")
        self.advance()  # Consume the closing '}'
        return AST.Block(statements)
    
    # TODO: Implement the logic to parse while loops with a condition and a block of statements
    def while_stmt(self):
        """
        Parses a while-statement.
        Example:
        while condition {
            # statements
        }
        TODO: Implement the logic to parse while loops with a condition and a block of statements.
        """
       
        if self.current_token[0] != 'WHILE':
            raise ValueError("Expected 'WHILE'")
        self.advance()

       
        condition = self.boolean_expression()

        if self.current_token[0] != 'LBRACE':
            raise ValueError("Expected '{'")
        self.advance()

        statements = []
        while self.current_token[0] != 'RBRACE':
            statements.append(self.statement())
            if self.current_token[0] == 'EOF':
                raise ValueError("Unexpected EOF")

        self.advance()  # Consume the closing '}'

        
        return AST.WhileStatement(condition, AST.Block(statements))

    # TODO: Implement logic to capture multiple statements as part of a block
    def block(self):
        """
        Parses a block of statements. A block is a collection of statements grouped by `{}`.
        Example:
        
        x = 5
        y = 10
        
        TODO: Implement logic to capture multiple statements as part of a block.
        """
        if self.current_token[0] == 'LBRACE':
            self.advance()
        else:
            raise ValueError("Expected {")

        statements = []
        while self.current_token[0] != 'RBRACE' and self.current_token[0] != 'EOF':
            statements.append(self.statement())

        if self.current_token[0] == 'EOF':
            raise ValueError("Expected }")

        self.advance()
        return AST.Block(statements)

    # TODO: Implement logic to parse binary operations (e.g., addition, subtraction) with correct precedence and type checking
    def expression(self):
        """
        Parses an expression. Handles operators like +, -, etc.
        Example:
        x + y - 5
        TODO: Implement logic to parse binary operations (e.g., addition, subtraction) with correct precedence and type checking.
        """
        left = self.term()
        
        while self.current_token and self.current_token[0] in ['PLUS', 'MINUS']:  
            operator = self.current_token[0]
            self.advance()  
            right_term = self.term()
            
           
            self.checkTypeMatch2(left.value_type, right_term.value_type, left, right_term)  
            
            
            left = AST.BinaryOperation(left, operator, right_term, value_type=left.value_type) 

        return left

    # TODO: Implement parsing for boolean expressions and check for type compatibility
    def boolean_expression(self):
        """
        Parses a boolean expression. These are comparisons like ==, !=, <, >.
        Example:
        x == 5
        TODO: Implement parsing for boolean expressions and check for type compatibility.
        """
        left_expression = self.expression()
        
        if self.current_token[0] in ['EQ', 'NEQ', 'LESS', 'GREATER']:
            operator = self.current_token[0]
            self.advance()
            right_expression = self.expression()
        else:
            raise ValueError(f"Expected a comparison operator, found: {self.current_token}")    

        
        if left_expression.value_type != right_expression.value_type:
            self.checkTypeMatch2(left_expression.value_type, right_expression.value_type, left_expression, right_expression)

        return AST.BooleanExpression(left_expression, operator, right_expression)
        

    # TODO: Implement parsing for multiplication and division and check for type compatibility
    def term(self):
        """
        Parses a term. A term consists of factors combined by * or /.
        Example:
        x * y / z
        TODO: Implement parsing for multiplication and division and check for type compatibility.
        """
        node = self.factor()

        
        while self.current_token[0] in ['MULTIPLY', 'DIVIDE']:
            op = self.current_token[0]
            self.advance()
            right = self.factor()

            
            self.checkTypeMatch2(node.value_type, right.value_type, node, right)

            
            node = AST.BinaryOperation(node, op, right, value_type=node.value_type)

        return node
    
    def factor(self):
        if self.current_token[0] == 'NUMBER':
           
            num = int(self.current_token[1])
            self.advance()
            return AST.Factor(num, 'int')
        elif self.current_token[0] == 'FNUMBER':
            
            num = float(self.current_token[1])
            self.advance()
            return AST.Factor(num, 'float')
        elif self.current_token[0] == 'IDENTIFIER':
            
            var_name = self.current_token[1]

            
            var_type = self.get_variable_type(var_name)
            if var_type is None:
                self.checkVarUse(var_name)  

            self.advance()
            return AST.Factor(var_name, var_type)
        elif self.current_token[0] == 'LPAREN':
            self.advance()
            expr = self.expression()
            self.expect('RPAREN')
            return expr
        else:
            raise ValueError(f"Unexpected token in factor: {self.current_token}")
        
    def function_call(self):
        func_name = self.current_token[1]
        self.advance()
        self.expect('LPAREN')
        args = self.arg_list()
        self.expect('RPAREN')

        return AST.FunctionCall(func_name, args)

    def arg_list(self):
        """
        Parses a list of function arguments.
        Example:
        (x, y + 5)
        """
        args = []
        if self.current_token[0] != 'RPAREN':
            args.append(self.expression())
            while self.current_token[0] == 'COMMA':
                self.advance()
                args.append(self.expression())

        return args

    def expect(self, token_type):
        if self.current_token[0] == token_type:
            self.advance()
        else:
            raise ValueError(f"Expected token {token_type}, but got {self.current_token[0]}")

    def peek(self):
        return self.tokens[0][0] if self.tokens else None