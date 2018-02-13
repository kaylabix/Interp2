#
# Class Interpreter 0
# Base interpreter with numbers, plus, and minus
#

module ExtInt

push!(LOAD_PATH, ".")

using Error
using Lexer
export parse, calc, interp

#
# ==================================================
#
function collatz( n::Real )
    return collatz_helper( n, 0 )
end

function collatz_helper( n::Real, num_iters::Int )
    if n == 1
        return num_iters
    end
    if mod(n,2)==0
        return collatz_helper( n/2, num_iters+1 )
    else
        return collatz_helper( 3*n+1, num_iters+1 )
    end
end

opDict = Dict(:+ => +, :- => -, :* => *, :/ => /, :mod => mod, :collatz => collatz)
keyWords = [:if0, :with, :lamda]

abstract type AE
end

type Num <: AE
    n::Real
end

type Binop <: AE
    op::Function
    lhs::AE
    rhs::AE
end

type UniOp <: AE
    op::Function
    operand::AE
end

type If0Node <: AE
    cond::AE
    zerobranch::AE
    nzerobranch::AE
end

type WithNode <: AE
    binding_expr::Array{Tuple{Symbol,AE}}  #array
    body::AE
end

type VarRefNode <: AE
    sym::Symbol
end

type FuncDefNode <: AE
    formal::Array #array
    body::AE
end

type FuncAppNode <: AE
    fun_expr::AE
    arg_expr::AE
end

#
# =================================================
#

abstract type RetVal
end

abstract type Environment
end

type NumVal <: RetVal
    n::Real
end

type ClosureVal <: RetVal
    formal::Symbol
    body::AE
    env::Environment
end

#
# ==================================================
#

type EmptyEnv <: Environment
end

type ExtendedEnv <: Environment
    sym::Symbol
    val::RetVal
    parent::Environment
end

#
# ==================================================
#

function parse( expr::Number )
    return Num( expr )
end

function parse( expr::Symbol )
    return VarRefNode( expr )
end

function parse(expr::Array{Any})
    if expr[1] in keys(opDict)
        if length(expr) == 3
            return Binop( get(opDict, expr[1], 0), parse( expr[2] ), parse(expr[3]) )
        elseif length(expr) == 2
            return UniOp( get(opDict, expr[1], 0), parse(expr[2]))
        end

    elseif expr[1] == :if0
        return If0Node(parse(expr[2]), parse(expr[3]) , parse(expr[4]))

    elseif expr[1] == :with
        binding_expr = expr[2]
        parsed_binding_expr = []
        if !(typeof(binding_expr) <: Array)
            throw(LispError("Unknown operator!"))
        end
        for i = 1:length(binding_expr)
            tuple = binding_expr[i]
            if length(tuple) != 2
                throw(LispError("Unknown operator!"))
            end
            if typeof(tuple[1]) != Symbol
                throw(LispError("Unknown operator!"))
            end
            # if keyWords.find(tuple[1]) == true
            #     throw(LispError("Unknown operator!"))
            # end
            push!(parsed_binding_expr, (tuple[1], parse(tuple[2])))
        end
        return WithNode(parsed_binding_expr, parse(expr[3]))

    elseif expr[1] == :lambda
        return FuncDefNode( expr[2], parse(expr[3]) )

    else
        return FuncAppNode( parse(expr[1]), parse(expr[2]) )

    end

    throw(LispError("Unknown operator!"))
end

function parse( expr::Any )
    throw( LispError("Invalid type $expr") )
end

#
# ==================================================
#

function binop_calc(x::NumVal, y::NumVal, op::Function)
    return NumVal(op(x.n, y.n))
end

function uniop_calc(x::NumVal, op::Function)
    return NumVal(op(x.n))
end

function calc(ast::Num, env::Environment)
    return NumVal(ast.n)
end

function calc( ast::Binop, env::Environment)
    lhs = calc(ast.lhs, env)
    rhs = calc(ast.rhs, env)
    if ast.op == /
        if rhs == 0
            throw(LispError("Can not divide by 0!"))
        end
    end
    return binop_calc(lhs, rhs, ast.op)
end

function calc(ast::UniOp, env::Environment)
    return uniop_calc(calc(ast.operand, env), ast.op)
end

function calc( ast::If0Node, env::Environment )
    cond = calc( ast.cond, env )
    if cond.n == 0
        return calc( ast.zerobranch, env )
    else
        return calc( ast.nzerobranch, env )
    end
end

function calc( ast::WithNode, env::Environment )
    binding_val = calc( ast.binding_expr, env )
    ext_env = ExtendedEnv( ast.sym, binding_val, env )
    return calc( ast.body, ext_env )
end

function calc( ast::VarRefNode, env::EmptyEnv )
    throw( Error.LispError("Undefined variable " * string( ast.sym )) )
end

function calc( ast::VarRefNode, env::ExtendedEnv )
    if ast.sym == env.sym
        return env.val
    else
        return calc( ast, env.parent )
    end
end

function calc( ast::FuncDefNode, env::Environment )
    return ClosureVal( ast.formal, ast.body , env )
end

function calc( ast::FuncAppNode, env::Environment )
    closure_val = calc( ast.fun_expr, env )
    actual_parameter = calc( ast.arg_expr, env )
    ext_env = ExtendedEnv( closure_val.formal,
    actual_parameter,
    closure_val.env )
    return calc( closure_val.body, ext_env )
end

function calc( ast::AE )
    return calc( ast, EmptyEnv() )
end


#
# ==================================================
#

function interp( cs::AbstractString )
    lxd = Lexer.lex( cs )
    return parse( lxd )
    return calc( ast, EmptyEnv() )
end

print(ExtInt.interp("(with ((x 6)(y 5)) (+ x y))"))

function interpf( fn::AbstractString )
    f = open( fn )

    cur_prog = ""
    for ln in eachline(f)
        ln = chomp( ln )
        if length(ln) == 0 && length(cur_prog) > 0
            println( "" )
            println( "--------- Evaluating ----------" )
            println( cur_prog )
            println( "---------- Returned -----------" )
            try
                println( interp( cur_prog ) )
            catch errobj
                println( ">> ERROR: lxd" )
                lxd = Lexer.lex( cur_prog )
                println( lxd )
                println( ">> ERROR: ast" )
                ast = parse( lxd )
                println( ast )
                println( ">> ERROR: rethrowing error" )
                throw( errobj )
            end
            println( "------------ done -------------" )
            println( "" )
            cur_prog = ""
        else
            cur_prog *= ln
        end
    end

    close( f )
end
end #module
