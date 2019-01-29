#!/bin/bash
cd ~/Poker
echo "clean ..." && stack clean && rm -rf .stack-work &&  \
echo "maketags ..." && maketags && \
echo "build ..." && stack build --haddock --test && \
echo "install ..." && stack install && \
echo -e "chunks ...\n"              && (poker chunks    | tail -10) && \
echo -e "-----------------------\n" && (poker chunks -r | tail -12) && \
echo "maketags again ..." && maketags && \
echo "graphs ..." && \
  (2>/dev/null graphmod | tee Poker.graph | dot -Tpdf > Poker.pdf) && open Poker.pdf && \
  (tred Poker.graph | tee PokerTred.graph | dot -Tpdf > PokerTred.pdf) && open PokerTred.pdf && \
success || failure
