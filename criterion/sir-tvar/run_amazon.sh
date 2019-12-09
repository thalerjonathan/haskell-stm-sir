#stack exec -- sir-tvar sir-tvar-cores:16/51x51 -o sir-tvar_amazon_51x51_16.html +RTS -N16

#stack exec -- sir-tvar sir-tvar-cores:32/51x51 -o sir-tvar_amazon_51x51_32.html +RTS -N32

stack exec -- sir-tvar sir-tvar-cores:64/51x51 -o sir-tvar_amazon_51x51_64.html +RTS -N64

#stack exec -- sir-tvar sir-tvar-agents:16/251x251 -o sir-tvar_amazon_251x251_16.html +RTS -N16

#stack exec -- sir-tvar sir-tvar-agents:32/251x251 -o sir-tvar_amazon_251x251_32.html +RTS -N32

stack exec -- sir-tvar sir-tvar-agents:64/251x251 -o sir-tvar_amazon_251x251_64.html +RTS -N64