#!/bin/bash

cd /home/janlo/Documents/literature
pdftk A=Lorenz2009Universalityofmovie.pdf B=Metz.Lorenz2013Becomewhoyou.pdf C=Lorenz2015ModelingEvolutionIdeological.pdf D=Groeber.Lorenz.ea2014Dissonanceminimizationas.pdf E=Shin.Lorenz2010TippingDiffusivityin.pdf F=Koenig.Lorenz.ea2016Innovationvsimitation.pdf G=Lorenz.Paetzel.ea2013Redistributionspursgrowth.pdf H=Lorenz.Paetzel.ea2016JustDontCall.pdf I=Lorenz.Rauhut.ea2011HowSocialInfluence.pdf J=Rauhut.Lorenz.ea2011ReplytoFarrell.pdf  K=Lorenz.Rauhut.ea2015MajoritarianDemocracyUndermines.pdf L=Lorenz.Rauhut.ea2015MajoritarianDemocracyUnderminesAppendix.pdf M=Rauhut.Lorenz2010wisdomofcrowds.pdf N=Lorenz2012ZurMethodeder.pdf cat N D2-end E2-end B A2-end C F G H M2-end I J K L output Habilitation_Work_Lorenz_Papers.pdf
mv Habilitation_Work_Lorenz_Papers.pdf /home/janlo/Documents/habil
cd /home/janlo/Documents/habil

gs \
    -o Habilitation_Work_Lorenz_Papers_resized.pdf \
    -sDEVICE=pdfwrite \
    -dPDFFitPage \
    -r300x300 \
    -g2100x2970 \
    Habilitation_Work_Lorenz_Papers.pdf

pdfbook habil.pdf


cd co-author_confirmations
pdftk Confirmation_JL_signed.pdf Confirmation_MDK_FZ_signed.pdf Confirmation_BK_signed.pdf Confirmation_PG_signed.pdf Confirmation_FP_MST_signed.pdf  Confirmations_signed.pdf
Confirmation_FS_signed.pdf Confirmation_TM_signed.pdf Confirmation_HR_signed.pdf Confirmation_JKS_signed.pdf 
 cat output ../co-author_contributions.pdf

cd ..
