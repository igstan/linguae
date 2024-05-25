#include <cctype>
#include <fstream>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <variant>
#include <vector>

/*

program         ::= {statement}
statement       ::= "let" identifier "=" expression ";" | "print" expression ";"
expression      ::= term {("+" | "-") term}
term            ::= factor {("*" | "/") factor}
factor          ::= number | identifier | "(" expression ")"
identifier      ::= letter {letter | digit}
number          ::= digit {digit}
letter          ::= "A" | "B" | ... | "Z" | "a" | "b" | ... | "z"
digit           ::= "0" | "1" | ... | "9"

*/

enum class TokenType {
    LET, PRINT, IDENTIFIER, NUMBER, PLUS, MINUS, MULTIPLY, DIVIDE, EQUAL, SEMICOLON, LPAREN, RPAREN
};

std::string token_type_to_string(TokenType type) {
    static const std::map<TokenType, std::string> token_type_map = {
        {TokenType::LET, "LET"},
        {TokenType::PRINT, "PRINT"},
        {TokenType::IDENTIFIER, "IDENTIFIER"},
        {TokenType::NUMBER, "NUMBER"},
        {TokenType::PLUS, "PLUS"},
        {TokenType::MINUS, "MINUS"},
        {TokenType::MULTIPLY, "MULTIPLY"},
        {TokenType::DIVIDE, "DIVIDE"},
        {TokenType::EQUAL, "EQUAL"},
        {TokenType::SEMICOLON, "SEMICOLON"},
        {TokenType::LPAREN, "LPAREN"},
        {TokenType::RPAREN, "RPAREN"}
    };

    auto it = token_type_map.find(type);
    if (it != token_type_map.end()) {
        return it->second;
    } else {
        throw std::runtime_error("Invalid TokenType value");
    }
}

struct Token {
    TokenType type;
    std::shared_ptr<std::string> input;
    std::size_t start;
    std::size_t length;

    std::string value() const {
        return input->substr(start, length);
    }

    std::string to_string() const {
        return "Token: {type=" + token_type_to_string(type) + ", value=" + value() + "}";
    }
};

class Lexer {
public:
    Lexer(std::shared_ptr<std::string> input) : input_(input), pos_(0) {}

    std::vector<Token> tokenize() {
        std::vector<Token> tokens;
        while (pos_ < input_->size()) {
            char c = (*input_)[pos_];
            if (std::isspace(c)) {
                ++pos_;
            } else if (std::isalpha(c)) {
                tokens.push_back(identifier());
            } else if (std::isdigit(c)) {
                tokens.push_back(number());
            } else {
                tokens.push_back(single_char_token());
            }
        }
        return tokens;
    }

private:
    Token identifier() {
        std::size_t start = pos_;
        while (pos_ < input_->size() && (std::isalnum((*input_)[pos_]) || (*input_)[pos_] == '_')) {
            ++pos_;
        }
        std::size_t length = pos_ - start;
        std::string value = input_->substr(start, length);

        TokenType type;
        if (value == "let") {
            type = TokenType::LET;
        } else if (value == "print") {
            type = TokenType::PRINT;
        } else {
            type = TokenType::IDENTIFIER;
        }

        return {type, input_, start, length};
    }

    Token number() {
        std::ostringstream os;
        int digit_count = 0;
        std::size_t start = pos_;
        const int max_digits = 10; // Assuming 32-bit integer, adjust as needed
        while (pos_ < input_->size() && std::isdigit((*input_)[pos_])) {
            if (digit_count++ >= max_digits) {
                throw std::runtime_error("Number too large");
            }
            os << input_->at(pos_++);
        }
        std::size_t length = pos_ - start;
        return {TokenType::NUMBER, input_, start, length};
    }

    Token single_char_token() {
        size_t start = pos_++;
        char c = (*input_)[start];
        return {single_char_token_type(c), input_, start, 1};
    }

    TokenType single_char_token_type(char c) {
        switch (c) {
            case '=':
                return TokenType::EQUAL;
            case ';':
                return TokenType::SEMICOLON;
            case '(':
                return TokenType::LPAREN;
            case ')':
                return TokenType::RPAREN;
            case '*':
                return TokenType::MULTIPLY;
            case '+':
                return TokenType::PLUS;
            case '-':
                return TokenType::MINUS;
            case '/':
                return TokenType::DIVIDE;
            default:
                throw std::runtime_error("Unexpected character: " + std::string(1, c));
        }
    }

    std::shared_ptr<std::string> input_;
    std::size_t pos_;
};

enum class BinOp {
    ADD, SUB, MUL, DIV
};

struct IntNode {
    int value;
};

struct VarNode {
    std::string name;
};

struct BinOpNode; // Forward declare BinOpNode here

using Expression = std::variant<IntNode, VarNode, BinOpNode>;

// Now define the BinOpNode struct
struct BinOpNode {
    BinOp op;
    std::unique_ptr<Expression> left;
    std::unique_ptr<Expression> right;
};

struct LetNode {
    std::string name;
    Expression value;
};

struct PrintNode {
    Expression value;
};

struct Compound;

using Statement = std::variant<PrintNode, LetNode, Compound>;

struct Compound {
    std::vector<Statement> statements;

    Compound(std::vector<Statement> stmts) : statements(std::move(stmts)) {}
};

class Parser {
public:
    Parser(const std::vector<Token>& tokens) : tokens_(tokens), pos_(0) {}

    Statement parse() {
        std::vector<Statement> statements = parse_statements();
        return Compound(std::move(statements));
    }

private:
    std::vector<Statement> parse_statements() {
        std::vector<Statement> statements;
        while (pos_ < tokens_.size()) {
            statements.push_back(parse_statement());
        }
        return statements;
    }

    Statement parse_statement() {
        if (match(TokenType::LET)) {
            std::string name = expect(TokenType::IDENTIFIER).value();
            expect(TokenType::EQUAL);
            Expression value = parse_expression();
            if (match(TokenType::SEMICOLON)) {
                // Consume the semicolon, but do nothing with it.
            }
            return LetNode{name, std::move(value)};
        } else if (match(TokenType::PRINT)) {
            Expression value = parse_expression();
            if (match(TokenType::SEMICOLON)) {
                // Consume the semicolon, but do nothing with it.
            }
            return PrintNode{std::move(value)};
        } else {
            throw std::runtime_error("Unexpected token in statement: " + current_token().value());
        }
    }

    Expression parse_expression() {
        return parse_binary_operation_rhs(0, parse_primary_expression());
    }

    Expression parse_primary_expression() {
        if (match(TokenType::NUMBER)) {
            Token matched_token = prev_token();
            int value = std::stoi(matched_token.value());
            return IntNode{value};
        } else if (match(TokenType::IDENTIFIER)) {
            Token matched_token = prev_token();
            std::string name = matched_token.value();
            return VarNode{name};
        } else if (match(TokenType::LPAREN)) {
            Expression expr = parse_expression();
            expect(TokenType::RPAREN);
            return expr;
        } else {
            throw std::runtime_error("Unexpected token in primary expression: " + current_token().value());
        }
    }

    const Token& prev_token() const {
        return tokens_[pos_ - 1];
    }

    Expression parse_binary_operation_rhs(int precedence, Expression lhs) {
        while (true) {
            int current_precedence = get_operator_precedence(current_token().type);
            if (current_precedence < precedence) {
                return lhs;
            }

            BinOp op = token_type_to_bin_op(current_token().type);
            ++pos_;
            Expression rhs = parse_primary_expression();
            int next_precedence = get_operator_precedence(current_token().type);
            if (current_precedence < next_precedence) {
                rhs = parse_binary_operation_rhs(current_precedence + 1, std::move(rhs));
            }
            lhs = BinOpNode{op, std::make_unique<Expression>(std::move(lhs)), std::make_unique<Expression>(std::move(rhs))};
        }
    }

    int get_operator_precedence(TokenType op) {
        switch (op) {
            case TokenType::PLUS:
            case TokenType::MINUS:
                return 1;
            case TokenType::MULTIPLY:
            case TokenType::DIVIDE:
                return 2;
            default:
                return -1;
        }
    }

    BinOp token_type_to_bin_op(TokenType type) {
        switch (type) {
            case TokenType::PLUS:
                return BinOp::ADD;
            case TokenType::MINUS:
                return BinOp::SUB;
            case TokenType::MULTIPLY:
                return BinOp::MUL;
            case TokenType::DIVIDE:
                return BinOp::DIV;
            default:
                throw std::runtime_error("Invalid binary operator token type");
        }
    }

    const Token& current_token() const {
        return tokens_[pos_];
    }

    bool match(TokenType type) {
        if (pos_ < tokens_.size() && tokens_[pos_].type == type) {
            ++pos_;
            return true;
        }
        return false;
    }

    Token expect(TokenType type) {
        if (!match(type)) {
            throw std::runtime_error("Unexpected token: " + current_token().to_string());
        }
        return tokens_[pos_ - 1];
    }

    const std::vector<Token>& tokens_;
    std::size_t pos_;
};

class AstToDotInterpreter {
public:
    AstToDotInterpreter() : node_id_(0) {
        output_ << "digraph AST {\n";
    }

    void visit_statements(const std::vector<Statement>& statements) {
        for (const auto& statement : statements) {
            std::visit(*this, statement);
        }
    }

    int operator()(const Compound& compound) {
        int current_id = node_id_++;
        output_ << "node" << current_id << " [label=\"Compound\"];\n";

        std::vector<int> child_ids;
        for (const auto& statement : compound.statements) {
            child_ids.push_back(std::visit(*this, statement));
        }

        for (const auto& child_id : child_ids) {
            output_ << "node" << current_id << " -> node" << child_id << ";\n";
        }
        return current_id;
    }

    int operator()(const LetNode& node) {
        int current_id = node_id_++;
        int child_id = std::visit(*this, node.value);

        output_ << "node" << current_id << " [label=\"Let: " << node.name << "\"];\n";
        output_ << "node" << current_id << " -> node" << child_id << ";\n";
        return current_id;
    }

    int operator()(const PrintNode& node) {
        int current_id = node_id_++;
        int child_id = std::visit(*this, node.value);

        output_ << "node" << current_id << " [label=\"Print\"];\n";
        output_ << "node" << current_id << " -> node" << child_id << ";\n";
        return current_id;
    }

    int operator()(const BinOpNode& node) {
        int current_id = node_id_++;
        int left_child_id = std::visit(*this, *node.left);
        int right_child_id = std::visit(*this, *node.right);

        output_ << "node" << current_id << " [label=\"" << bin_op_to_string(node.op) << "\"];\n";
        output_ << "node" << current_id << " -> node" << left_child_id << ";\n";
        output_ << "node" << current_id << " -> node" << right_child_id << ";\n";
        return current_id;
    }

    int operator()(const IntNode& node) {
        int current_id = node_id_++;
        output_ << "node" << current_id << " [label=\"" << node.value << "\"];\n";
        return current_id;
    }

    int operator()(const VarNode& node) {
        int current_id = node_id_++;
        output_ << "node" << current_id << " [label=\"" << node.name << "\"];\n";
        return current_id;
    }

    std::string get_output() const {
        return output_.str() + "}\n";
    }

private:
    std::string bin_op_to_string(BinOp op) {
        switch (op) {
            case BinOp::ADD: return "+";
            case BinOp::SUB: return "-";
            case BinOp::MUL: return "*";
            case BinOp::DIV: return "/";
            default: return "Unknown";
        }
    }

    std::stringstream output_;
    int node_id_;
};

class Evaluator {
public:
    void visit_statements(const std::vector<Statement>& statements) {
        for (const auto& statement : statements) {
            std::visit(*this, statement);
        }
    }

    void operator()(const Compound& compound) {
        visit_statements(compound.statements);
    }

    void operator()(const LetNode& node) {
        auto value = std::visit(*this, node.value);
        variables_[node.name] = value;
    }

    void operator()(const PrintNode& node) {
        auto value = std::visit(*this, node.value);
        std::cout << value << std::endl;
    }

    int operator()(const BinOpNode& node) {
        int left_value = std::visit(*this, *node.left);
        int right_value = std::visit(*this, *node.right);

        switch (node.op) {
            case BinOp::ADD:
                return left_value + right_value;
            case BinOp::SUB:
                return left_value - right_value;
            case BinOp::MUL:
                return left_value * right_value;
            case BinOp::DIV:
                if (right_value == 0) {
                    throw std::runtime_error("Division by zero");
                }
                return left_value / right_value;
            default:
                throw std::runtime_error("Unknown binary operator");
        }
    }

    int operator()(const IntNode& node) {
        return node.value;
    }

    int operator()(const VarNode& node) {
        auto it = variables_.find(node.name);
        if (it == variables_.end()) {
            throw std::runtime_error("Undefined variable: " + node.name);
        }
        return it->second;
    }

private:
    std::unordered_map<std::string, int> variables_;
};

class Aarch64Compiler {
public:
    Aarch64Compiler() {
        output_ << ".text\n";
        output_ << ".global _main\n";
        output_ << ".align 4\n";
        output_ << ".extern _print_int\n";
        output_ << "_main:\n";
        output_ << "stp x29, x30, [sp, -16]!\n";
        output_ << "mov x29, sp\n";
        output_ << "sub sp, sp, 1024\n";
    }

    void visit_statements(const std::vector<Statement>& statements) {
        for (const auto& statement : statements) {
            std::visit(*this, statement);
        }
    }

    void operator()(const Compound& compound) {
        visit_statements(compound.statements);
    }

    void operator()(const LetNode& node) {
        // Generate assembly code for variable assignment
        int offset = get_variable_offset(node.name);
        std::visit(*this, node.value);
        output_ << "str w0, [x29, " << offset << "]\n";
    }

    void operator()(const PrintNode& node) {
        // Generate assembly code for the print statement
        std::visit(*this, node.value);
        output_ << "mov w1, w0\n";
        output_ << "bl _print_int\n";
    }

    void operator()(const BinOpNode& node) {
        // Generate assembly code for the binary operation
        std::visit(*this, *node.left);
        output_ << "mov w1, w0\n";
        std::visit(*this, *node.right);
        output_ << "mov w2, w0\n";

        switch (node.op) {
            case BinOp::ADD:
                output_ << "add w0, w1, w2\n";
                break;
            case BinOp::SUB:
                output_ << "sub w0, w1, w2\n";
                break;
            case BinOp::MUL:
                output_ << "mul w0, w1, w2\n";
                break;
            case BinOp::DIV:
                output_ << "sdiv w0, w1, w2\n";
                break;
        }
    }

    void operator()(const IntNode& node) {
        // Generate assembly code for the integer value
        output_ << "mov w0, " << node.value << "\n";
    }

    void operator()(const VarNode& node) {
        // Generate assembly code for the variable reference
        int offset = get_variable_offset(node.name);
        output_ << "ldr w0, [x29, " << offset << "]\n";
    }

    std::string get_output() const {
        std::stringstream final_output;
        final_output << output_.str();
        final_output << "mov sp, x29\n";
        final_output << "ldp x29, x30, [sp], 16\n";
        final_output << "mov w0, 0\n";
        final_output << "ret\n";
        return final_output.str();
    }

private:
    int get_variable_offset(const std::string& name) {
        if (variables_.find(name) == variables_.end()) {
            variables_[name] = (variables_.size() + 1) * 8 + 8; // Start with an offset of 16
        }
        return variables_[name];
    }

    std::stringstream output_;
    std::unordered_map<std::string, int> variables_;
};

int main() {
    auto input = std::make_shared<std::string>(
        "let x = 1 + 2; let y = x * 3; print y;"
        // "let a = 10;\n"
        // "let b = a * 2;\n"
        // "print b;\n"
        // "print a + b / 2;\n"
    );
    Lexer lexer(input);
    auto tokens = lexer.tokenize();
    Parser parser(tokens);
    auto ast_root = parser.parse();

    // Evaluator evaluator;
    // std::visit(evaluator, ast_root);

    // AstToDotInterpreter interpreter;
    // std::visit(interpreter, ast_root);
    // std::string dot_output = interpreter.get_output();
    // std::cout << dot_output;

    Aarch64Compiler compiler;
    std::visit(compiler, ast_root);

    std::string assembly_code = compiler.get_output();
    std::cout << assembly_code;
}


// int main() {
//     auto input = std::make_shared<std::string>(
//         "let a = 10;\n"
//         "let b = a * 2;\n"
//         "print b;\n"
//         "print a + b / 2;\n"
//     );

//     Lexer lexer(input);
//     auto tokens = lexer.tokenize();

//     Parser parser(tokens);
//     auto ast = parser.parse();

//     Evaluator evaluator;
//     std::visit(evaluator, ast);

//     return 0;
// }

// int main() {
//     auto input = std::make_shared<std::string>("let x = 10; print (x * 2);");
//     Lexer lexer(input);
//     std::vector<Token> tokens = lexer.tokenize();

//     for (const Token& token : tokens) {
//         std::cout << token.to_string() << "\n";
//     }

//     Parser parser(tokens);
//     Statement ast = parser.parse();

//     AstToDotInterpreter interpreter;
//     std::visit(interpreter, ast);
//     std::string dot_output = interpreter.get_output();

//     std::ofstream output_file("ast.dot");
//     if (output_file.is_open()) {
//         output_file << dot_output;
//         std::cout << "AST has been written to ast.dot\n";
//     } else {
//         std::cerr << "Error: Unable to open file ast.dot for writing\n";
//     }

//     return 0;
// }
