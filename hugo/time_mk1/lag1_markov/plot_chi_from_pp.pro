; .run plot_chi_from_pp.pro

;stin = 'hw_models/gem2/DurSevFromObs_ajayoa_ant_europe_bigger.pp'
;pp1 = ppa(stin,/all)

stland = '/project/extremes1/hadsx/ibackup/masks/hadgem2_land.pp'
ppland = ppa(stland)
pplandeu = pp_extract(ppland,[-12,33.75,37.5,61.25])
;WAVEOFF-TIDL> get_coords_from_plot,pplandeu(0)
;
;bot l Value ********, (x,y)   0   0, (lat,long)   33.54  -11.81
;bot r Value   14.528, (x,y)  26   0, (lat,long)   33.62   38.06
;top l Value   42.862, (x,y)  26  22, (lat,long)   61.66   38.06
;top r Value ********, (x,y)   0  22, (lat,long)   61.51  -11.81


ukcp

;!p.multi = [0,1,2]
;plot_sjb_block,pp1(0),nlev=41,/dont,maxmin=[0.65,1.06],tit='chi forward'
;plot_sjb_block,pp1(1),nlev=41,/dont,maxmin=[0.65,1.06],tit='chi backward'
;pp2 = pp_ff('a',pp1,pplandeu)
;plot_sjb_block,pp2(0),nlev=41,/dont,maxmin=[0.65,1.06],tit='chi forward'
;plot_sjb_block,pp2(1),nlev=41,/dont,maxmin=[0.65,1.06],tit='chi backward'

fant  = findfile('hw_models/gem2/DurSevFromObs_*_ant_europe_bigger.pp')
ppant = ppa(fant[0],/all)    
for f=1,(f_ne(fant)-1) do begin
  pp1   = ppa(fant[f],/all)    
  ppant = [ppant,pp1]
endfor
ppant = pp_ff('a',ppant,pplandeu)

fghg  = findfile('hw_models/gem2/DurSevFromObs_*_ghg_europe_bigger.pp')
ppghg = ppa(fghg[0],/all)    
for f=1,(f_ne(fghg)-1) do begin
  pp1   = ppa(fghg[f],/all)    
  ppghg = [ppghg,pp1]
endfor
ppghg = pp_ff('a',ppghg,pplandeu)

fnat  = findfile('hw_models/gem2/DurSevFromObs_*_nat_europe_bigger.pp')
ppnat = ppa(fnat[0],/all)    
for f=1,(f_ne(fnat)-1) do begin
  pp1   = ppa(fnat[f],/all)    
  ppnat = [ppnat,pp1]
endfor
ppnat = pp_ff('a',ppnat,pplandeu)

pr,/cps,/land,file='chi_all_runs.ps'
; 4x2 plots of chi fwd and bwd
!p.multi = [0,4,3]
!p.charsize = 0
mm1 = [0.1,0.9]
mm2 = [-0.2,0.2]
    ; ANT
    plot_sjb_block,ppant(0),nlev=41,/dont,maxmin=mm1,tit='chi forward: ANT1'
    plot_sjb_block,ppant(2),nlev=41,/dont,maxmin=mm1,tit='chi forward: ANT2'
    plot_sjb_block,ppant(4),nlev=41,/dont,maxmin=mm1,tit='chi forward: ANT3'
    plot_sjb_block,ppant(6),nlev=41,/dont,maxmin=mm1,tit='chi forward: ANT4'
    plot_sjb_block,ppant(1),nlev=41,/dont,maxmin=mm1,tit='chi backward: ANT1'
    plot_sjb_block,ppant(3),nlev=41,/dont,maxmin=mm1,tit='chi backward: ANT2'
    plot_sjb_block,ppant(5),nlev=41,/dont,maxmin=mm1,tit='chi backward: ANT3'
    plot_sjb_block,ppant(7),nlev=41,/dont,maxmin=mm1,tit='chi backward: ANT4'
    plot_sjb_block,pp_diff(ppant(0),ppant(1)),nlev=41,maxmin=mm2,tit='chi forward-backward: ANT1'
    plot_sjb_block,pp_diff(ppant(2),ppant(3)),nlev=41,maxmin=mm2,tit='chi forward-backward: ANT2'
    plot_sjb_block,pp_diff(ppant(4),ppant(5)),nlev=41,maxmin=mm2,tit='chi forward-backward: ANT3'
    plot_sjb_block,pp_diff(ppant(6),ppant(7)),nlev=41,maxmin=mm2,tit='chi forward-backward: ANT4'

    ; GHG
    plot_sjb_block,ppghg(0),nlev=41,/dont,maxmin=mm1,tit='chi forward: GHG1'
    plot_sjb_block,ppghg(2),nlev=41,/dont,maxmin=mm1,tit='chi forward: GHG2'
    plot_sjb_block,ppghg(4),nlev=41,/dont,maxmin=mm1,tit='chi forward: GHG3'
    plot_sjb_block,ppghg(6),nlev=41,/dont,maxmin=mm1,tit='chi forward: GHG4'
    plot_sjb_block,ppghg(1),nlev=41,/dont,maxmin=mm1,tit='chi backward: GHG1'
    plot_sjb_block,ppghg(3),nlev=41,/dont,maxmin=mm1,tit='chi backward: GHG2'
    plot_sjb_block,ppghg(5),nlev=41,/dont,maxmin=mm1,tit='chi backward: GHG3'
    plot_sjb_block,ppghg(7),nlev=41,/dont,maxmin=mm1,tit='chi backward: GHG4'
    plot_sjb_block,pp_diff(ppghg(0),ppghg(1)),nlev=41,maxmin=mm2,tit='chi forward-backward: GHG1'
    plot_sjb_block,pp_diff(ppghg(2),ppghg(3)),nlev=41,maxmin=mm2,tit='chi forward-backward: GHG2'
    plot_sjb_block,pp_diff(ppghg(4),ppghg(5)),nlev=41,maxmin=mm2,tit='chi forward-backward: GHG3'
    plot_sjb_block,pp_diff(ppghg(6),ppghg(7)),nlev=41,maxmin=mm2,tit='chi forward-backward: GHG4'

    ; NAT
    plot_sjb_block,ppnat(0),nlev=41,/dont,maxmin=mm1,tit='chi forward: NAT1'
    plot_sjb_block,ppnat(2),nlev=41,/dont,maxmin=mm1,tit='chi forward: NAT2'
    plot_sjb_block,ppnat(4),nlev=41,/dont,maxmin=mm1,tit='chi forward: NAT3'
    plot_sjb_block,ppnat(6),nlev=41,/dont,maxmin=mm1,tit='chi forward: NAT4'
    plot_sjb_block,ppnat(1),nlev=41,/dont,maxmin=mm1,tit='chi backward: NAT1'
    plot_sjb_block,ppnat(3),nlev=41,/dont,maxmin=mm1,tit='chi backward: NAT2'
    plot_sjb_block,ppnat(5),nlev=41,/dont,maxmin=mm1,tit='chi backward: NAT3'
    plot_sjb_block,ppnat(7),nlev=41,/dont,maxmin=mm1,tit='chi backward: NAT4'
    plot_sjb_block,pp_diff(ppnat(0),ppnat(1)),nlev=41,maxmin=mm2,tit='chi forward-backward: NAT1'
    plot_sjb_block,pp_diff(ppnat(2),ppnat(3)),nlev=41,maxmin=mm2,tit='chi forward-backward: NAT2'
    plot_sjb_block,pp_diff(ppnat(4),ppnat(5)),nlev=41,maxmin=mm2,tit='chi forward-backward: NAT3'
    plot_sjb_block,pp_diff(ppnat(6),ppnat(7)),nlev=41,maxmin=mm2,tit='chi forward-backward: NAT4'

; averages
aantf = pp_avg(ppant[[0,2,4,6]])
aantb = pp_avg(ppant[[1,3,5,7]])
aghgf = pp_avg(ppghg[[0,2,4,6]])
aghgb = pp_avg(ppghg[[1,3,5,7]])
anatf = pp_avg(ppnat[[0,2,4,6]])
anatb = pp_avg(ppnat[[1,3,5,7]])
!p.multi = [0,3,2]
    plot_sjb_block,aantf,nlev=41,/dont,maxmin=mm1,tit='chi forward: avg ANT'
    plot_sjb_block,aghgf,nlev=41,/dont,maxmin=mm1,tit='chi forward: avg GHG'
    plot_sjb_block,anatf,nlev=41,/dont,maxmin=mm1,tit='chi forward: avg NAT'
    plot_sjb_block,aantb,nlev=41,/dont,maxmin=mm1,tit='chi backward: avg ANT'
    plot_sjb_block,aghgb,nlev=41,/dont,maxmin=mm1,tit='chi backward: avg GHG'
    plot_sjb_block,anatb,nlev=41,/dont,maxmin=mm1,tit='chi backward: avg NAT'
; diffs
    plot_sjb_block,pp_diff(aantf,aghgf),nlev=41,/dont,maxmin=mm2,tit='chi forward: aANT-aGHG'
    plot_sjb_block,pp_diff(aantf,anatf),nlev=41,/dont,maxmin=mm2,tit='chi forward: aANT-aNAT'
    plot_sjb_block,pp_diff(anatf,aghgf),nlev=41,/dont,maxmin=mm2,tit='chi forward: aNAT-aGHG'

    plot_sjb_block,pp_diff(aantb,aghgb),nlev=41,/dont,maxmin=mm2,tit='chi backward: aANT-aGHG'
    plot_sjb_block,pp_diff(aantb,anatb),nlev=41,/dont,maxmin=mm2,tit='chi backward: aANT-aNAT'
    plot_sjb_block,pp_diff(anatb,aghgb),nlev=41,/dont,maxmin=mm2,tit='chi backward: aNAT-aGHG'

prend,/view,/noprint

end
