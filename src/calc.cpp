#include "calc.h"

#include <cctype>   // for std::isspace
#include <cmath>    // various math functions
#include <iostream> // for error reporting via std::cerr
#include <sstream>

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

Op parse_op(const std::string & line, std::size_t & i)
{
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
    good = true;
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
    good = true;
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

} // anonymous namespace

std::size_t skip_brackets(const std::string & line, std::size_t i)
{
    while (i < line.size()) {
        i++;
        if (line[i] == ')') {
            i++;
            break;
        }
    }
    return i;
}

std::size_t parse_number(const std::string & line, std::size_t i)
{
    size_t count = 0;
    while (i < line.size() && !std::isspace(line[i])) {
        i++;
        count++;
    }
    return count;
}

double left_fold(double current, const std::string & line)
{
    const auto old_current = current;
    std::size_t i = 0;
    std::size_t pos_binary_op = 1;
    bool good;
    const auto op = parse_op(line, pos_binary_op);
    if ((arity(op) != 2) || (op == Op::SET)) {
        std::cerr << "Left convolutional is work only binary opertion" << std::endl;
        return current;
    }
    i = skip_brackets(line, i);
    i = skip_ws(line, i);
    if (i == line.size()) {
        std::cerr << "No arguments" << std::endl;
        return current;
    }
    while (i < line.size()) {
        const auto number_size = parse_number(line, i);
        std::size_t pos = 0;
        const auto arg = parse_arg(line.substr(i, number_size), pos, good);
        if (!good) {
            std::cerr << "Wrong arguments" << std::endl;
            return old_current;
        }
        current = binary(op, current, arg, good);
        if (!good) {
            return old_current;
        }
        i += number_size;
        i = skip_ws(line, i);
    }
    return current;
}

double process_line(const double current, const std::string & line)
{
    if ((line[0] == '(') && (line[2] == ')')) {
        return left_fold(current, line);
    }
    else {
        std::size_t i = 0;
        const auto op = parse_op(line, i);
        switch (arity(op)) {
        case 2: {
            i = skip_ws(line, i);
            const auto old_i = i;
            bool good;
            const auto arg = parse_arg(line, i, good);
            if (i == old_i) {
                std::cerr << "No argument for a binary operation" << std::endl;
                break;
            }
            else if (i < line.size()) {
                break;
            }
            return binary(op, current, arg, good);
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
}
