#include "calc.h"

#include <cctype>   // for std::isspace
#include <cmath>    // various math functions
#include <iostream> // for error reporting via std::cerr
#include <vector>
namespace {

const std::size_t max_decimal_digits = 10;

enum class Op
{
    ERR,
    SET,
    ADD,
    SUB,
    MUL,
    DIV,
    REM,
    NEG,
    POW,
    SQRT
};

std::size_t arity(const Op op)
{
    switch (op) {
    // error
    case Op::ERR: return 0;
    // unary
    case Op::NEG: return 1;
    case Op::SQRT: return 1;
    // binary
    case Op::SET: return 2;
    case Op::ADD: return 2;
    case Op::SUB: return 2;
    case Op::MUL: return 2;
    case Op::DIV: return 2;
    case Op::REM: return 2;
    case Op::POW: return 2;
    }
    return 0;
}

std::string delete_brackets(const std::string & line)
{
    if (line[0] == '(') {
        for (std::size_t i = 0; i < line.size(); i++) {
            if (line[i] == ')') {
                return line.substr(1, i - 1) + line.substr(i + 1, line.size() - i + 1);
            }
            if (std::isspace(line[i])) {
                return line;
            }
        }
        return line;
    }
    return line;
}

Op parse_op(const std::string & line_raw, std::size_t & i)
{
    std::string line = delete_brackets(line_raw);
    const auto rollback = [&i, &line](const std::size_t n) {
        i -= n;
        std::cerr << "Unknown operation " << line << std::endl;
        return Op::ERR;
    };
    switch (line[i++]) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
        --i; // a first digit is a part of op's argument
        return Op::SET;
    case '+':
        return Op::ADD;
    case '-':
        return Op::SUB;
    case '*':
        return Op::MUL;
    case '/':
        return Op::DIV;
    case '%':
        return Op::REM;
    case '_':
        return Op::NEG;
    case '^':
        return Op::POW;
    case 'S':
        switch (line[i++]) {
        case 'Q':
            switch (line[i++]) {
            case 'R':
                switch (line[i++]) {
                case 'T':
                    return Op::SQRT;
                default:
                    return rollback(4);
                }
            default:
                return rollback(3);
            }
        default:
            return rollback(2);
        }
    default:
        return rollback(1);
    }
}

std::size_t skip_ws(const std::string & line, std::size_t i)
{
    while (i < line.size() && std::isspace(line[i])) {
        ++i;
    }
    return i;
}

double parse_arg(const std::string & line, std::size_t & i, bool & good)
{
    double res = 0;
    std::size_t count = 0;
    bool integer = true;
    double fraction = 1;
    while (good && i < line.size() && count < max_decimal_digits) {
        switch (line[i]) {
        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9':
            if (integer) {
                res *= 10;
                res += line[i] - '0';
            }
            else {
                fraction /= 10;
                res += (line[i] - '0') * fraction;
            }
            ++i;
            ++count;
            break;
        case '.':
            integer = false;
            ++i;
            break;
        default:
            good = false;
            break;
        }
    }
    if (!good) {
        std::cerr << "Argument parsing error at " << i << ": '" << line.substr(i) << "'" << std::endl;
    }
    else if (i < line.size()) {
        good = false;
        std::cerr << "Argument isn't fully parsed, suffix left: '" << line.substr(i) << "'" << std::endl;
    }
    return res;
}

double unary(const double current, const Op op)
{
    switch (op) {
    case Op::NEG:
        return -current;
    case Op::SQRT:
        if (current > 0) {
            return std::sqrt(current);
        }
        else {
            std::cerr << "Bad argument for SQRT: " << current << std::endl;
            [[fallthrough]];
        }
    default:
        return current;
    }
}

double binary(const Op op, const double left, const double right, bool & good)
{
    switch (op) {
    case Op::SET:
        return right;
    case Op::ADD:
        return left + right;
    case Op::SUB:
        return left - right;
    case Op::MUL:
        return left * right;
    case Op::DIV:
        if (right != 0) {
            return left / right;
        }
        else {
            good = false;
            std::cerr << "Bad right argument for division: " << right << std::endl;
            return left;
        }
    case Op::REM:
        if (right != 0) {
            return std::fmod(left, right);
        }
        else {
            good = false;
            std::cerr << "Bad right argument for remainder: " << right << std::endl;
            return left;
        }
    case Op::POW:
        return std::pow(left, right);
    default:
        return left;
    }
}

std::size_t skip_brackets(const std::string & line, std::size_t i)
{
    const auto old_i = i;
    if (line[0] == '(') {
        for (; i < line.size(); i++) {
            if (line[i] == ')') {
                return ++i;
            }
        }
        return old_i;
    }
    else
        return old_i;
}

bool is_fold(const std::string & line)
{
    if (line[0] == '(') {
        for (const auto & ch : line) {
            if (ch == ')') {
                return true;
            }
        }
        return false;
    }
    else
        return false;
}

std::size_t parse_number_length(const std::string & line, std::size_t i)
{
    std::size_t count = 0;
    while (i < line.size() && !std::isspace(line[i])) {
        i++;
        count++;
    }
    return count;
}

std::vector<std::string> parse_number(const std::string & line, std::size_t i)
{
    std::vector<std::string> numbers;
    while (i < line.size()) {
        i = skip_ws(line, i);
        auto number_length = parse_number_length(line, i);
        if (number_length != 0) {
            numbers.push_back(line.substr(i, number_length));
        }
        i += number_length;
    }
    if (numbers.empty()) {
        std::cerr << "No argument for a binary operation" << std::endl;
    }
    return numbers;
}

} // anonymous namespace

double process_line(const double current, const std::string & line)
{
    std::size_t i = 0;
    const auto op = parse_op(line, i);
    switch (arity(op)) {
    case 2: {
        auto res = current;
        i = skip_brackets(line, i);
        std::vector<std::string> numbers = parse_number(line, i);
        bool good = true;
        for (const auto & str_number : numbers) {
            double arg;
            // проверка для обхода пробельного бага в тесте : '+ 1 '
            if (is_fold(line)) {
                if (op == Op::SET) {
                    std::cerr << "Wrong operation left fold" << std::endl;
                    return current;
                }
                i = 0;
                arg = parse_arg(str_number, i, good);
            }
            else {
                i = skip_ws(line, i);
                const auto old_i = i;
                arg = parse_arg(line, i, good);
                //также эта проверка только из-за теста, где для случая '+ -'и др. требуется два cerr: Parsing Err и No arguments
                //хотя я думаю, что только ошибки парсинга было бы достаточно
                if (i == old_i) {
                    std::cerr << "No argument for a binary operation" << std::endl;
                    return current;
                }
            }
            res = binary(op, res, arg, good);
            if (!good) {
                return current;
            }
        }
        return res;
    }
    case 1: {
        if (i < line.size()) {
            std::cerr << "Unexpected suffix for a unary operation: '" << line.substr(i) << "'" << std::endl;
            break;
        }
        return unary(current, op);
    }
    default: break;
    }
    return current;
}
