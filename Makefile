IFORT = 1

include $(WISE_MAKEINC)/stdheader.mk
include $(WISE_MAKEINC)/stdmacros.mk

EXECLIST = mdet
FSUBS = mparg.f mreadimage.f shuffle.f headpar.f segmenter.f stringstuff.f mwimage.f writeimage.f svb.f svb_mf.f sort.f medsort.f groupsort.f gausscalc.f findpeaks.f weedout.f weedout_mf.f appendlist.f pix2eq.f estmode.f

ALL = bin

FOPT = -O3

include $(WISE_MAKEINC)/stdtargets.mk
