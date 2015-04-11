#!/bin/sh
nohup nice +8 R --no-save < runmod_base.R >& output_base.Rout &
nohup nice +8 R --no-save < runmod_allpart.R >& output_allpart.Rout &
nohup nice +8 R --no-save < runmod_indpart.R >& output_indpart.Rout &
nohup nice +8 R --no-save < runmod_dirpart.R >& output_dirpart.Rout &
nohup nice +8 R --no-save < runmod_shapart.R >& output_shapart.Rout &
nohup nice +8 R --no-save < runmod_pasttie.R >& output_pasttie.Rout &
nohup nice +8 R --no-save < grab_modobjects.R >& output_grabmods.Rout &
nohup nice +8 R --no-save < make_diag_plots.R >& output_makeplots.Rout &
nohup nice +8 R --no-save < make_paper_tables.R >& output_maketables.Rout &