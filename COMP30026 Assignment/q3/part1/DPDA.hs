WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
module DPDA
where
----------------------------------------------------------------------
--
--  ﻿﻿﻿A type definition for DPDAs, deterministic pushdown automata
--
----------------------------------------------------------------------

type DPDA     = ([State], [Symbol], [StackSym], [Transn], State, [State])
type Transn   = ((State, Symbol, StackSym), (State, StackSym))
type State    = Integer
type Symbol   = Char
type StackSym = Char
type Stack    = [StackSym]
type Input    = [Symbol]

epsilon :: StackSym
epsilon
  = 'ε'

-- for short:
eps :: StackSym
eps
  = epsilon