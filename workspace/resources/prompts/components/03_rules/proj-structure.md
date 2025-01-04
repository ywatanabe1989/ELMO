<!-- ---
!-- title: 2025-01-04 18:43:51
!-- author: Yusuke Watanabe
!-- date: /home/ywatanabe/proj/llemacs/workspace/resources/prompts/components/03_rules/proj-structure.md
!-- --- -->

# Rule: proj-structure
* The ID and name of the current project is pre-defined as `llemacs--cur-pj` (e.g., `000-sample-project`).
* Paths to directory and files for this project workspace is pre-defined as Elisp variables as shown below.
* For example, this is the default project structure:
/path/to/llemacs/workspace/projects/`llemacs--cur-pj`/ (= `llemacs--path-pj`)
├── .env (= `llemacs--path-pj-python-env`)
│   ├── bin/python (= `llemacs--path-pj-python`)
├── config (= `llemacs--path-pj-config`)
├── data (= `llemacs--path-pj-data`)
├── logs (= `llemacs--path-pj-logs`)
│   ├── by_level (= `llemacs--path-pj-logs-by-level`)
│   │   ├── debug.log (Updated using `(llemacs--logging-write-debug-pj message)`)
│   │   ├── elisp.log (Updated using `(llemacs--logging-write-elisp-pj message)`)
│   │   ├── error.log (Updated using `(llemacs--logging-write-error-pj message)`)
│   │   ├── info.log (Updated using `(llemacs--logging-write-info-pj message)`)
│   │   ├── prompt.log (Updated using `(llemacs--logging-write-prompt-pj message)`)
│   │   ├── search.log (Updated using `(llemacs--logging-write-search-pj message)`)
│   │   └── warn.log (Updated using `(llemacs--logging-write-warn-pj message)`)
│   └── logging.log (Includes all the level-specific logs)
├── project-management
│   ├── project-management.gif
│   ├── project-management.mmd (= `llemacs--path-pj-project-management`)
│   ├── project-management.png
│   └── project-management.svg
├── README.md
├── requirements.txt
├── results (= `llemacs--path-pj-results`)
└── scripts (= `llemacs--path-pj-scripts`)
* Edit project directory organization appropriately.
* Current project tree will be included as part of prompt, as context.
* For example, I am am fond of this kind of organization:
```plaintext
.
├── config
│   ├── README.md
│   ├── COLORS.yaml
│   ├── DSP.yaml
│   ├── ECOG.yaml
│   ├── GLOBAL.yaml
│   ├── IS_DEBUG.yaml
│   ├── ML.yaml
│   ├── PAC.yaml
│   ├── PATH.yaml
│   ├── PATIENTS_ALL.yaml
│   ├── PATIENTS.yaml
│   ├── REPRESENTATIVES.yaml
│   ├── SAMPLING.yaml
│   └── SEIZURE.yaml
├── data -> .data/spartan
│     ├── README.md
│     ├── db
│     │   ├── ecog
│     │   ├── mat
│     │   ├── meta
│     │   └── pac
│     ├── mat
│     │   └── Annotations
│     ├── mat_list -> ../../scripts/dataset/list_mat_files
│     ├── README.md
│     └── seizures
│         ├── all.csv -> ../../../scripts/dataset/inspect_seizure_annotations/data/seizures/all.csv
│          ├── described.csv -> ../../../scripts/dataset/inspect_seizure_annotations/data/seizures/described.csv
│          ├── duration_by_type.csv -> ../../../scripts/dataset/inspect_seizure_annotations/data/seizures/duration_by_type.csv                        
│          ├── hourly_dist.jpg -> ../../../scripts/dataset/inspect_seizure_annotations/data/seizures/hourly_dist.jpg
│          ├── hours_since_last.jpg -> ../../../scripts/dataset/inspect_seizure_annotations/data/seizures/hours_since_last.jpg                        
│          ├── hours_since_last_log.jpg -> ../../../scripts/dataset/inspect_seizure_annotations/data/seizures/hours_since_last_log.jpg                    
│          ├── patient_seizures.csv -> ../../../scripts/dataset/inspect_seizure_annotations/data/seizures/patient_seizures.csv                        
│          ├── seizure_count.jpg -> ../../../scripts/dataset/inspect_seizure_annotations/data/seizures/seizure_count.jpg                           
│          ├── seizure_count_safe.csv -> ../../../scripts/dataset/inspect_seizure_annotations/seizure_count_safe.csv
│          ├── seizure_duration_hist.jpg -> ../../../scripts/dataset/inspect_seizure_annotations/data/seizures/seizure_duration_hist.jpg                   
│          ├── seizure_raster.jpg -> ../../../scripts/dataset/inspect_seizure_annotations/data/seizures/seizure_raster.jpg                          
├── docs
│   ├── dataset.md
│   ├── installation.md
│   └── memo.md
├── models
│   ├── best_model_23_004.pt
│   └── hub
├── paper
│   ├── compile -> scripts/compile-all.sh
│   ├── compile.log
│   ├── data
│   ├── manuscript
│   ├── README.md
│   ├── revision
│   ├── scripts
│   └── supplementary
├── README.md
└── scripts
    ├── dataset
    │   ├── check_disk_usage_and_du.log
    │   ├── check_disk_usage_and_du.sh
    │   ├── check_number_of_mat_files.sh
    │   ├── check_number_of_mat_files.sh.log
    │   ├── check_number_of_mat_files.sh.logs
    │   ├── create_mat_files_list
    │   ├── define_control_seizures.py
    │   ├── demographic_data.py
    │   ├── extract_patient_directories.sh
    │   ├── inspect_annotations
    │   ├── inspect_dropout.py
    │   ├── inspect_intertimes.py
    │   ├── _inspect_metadata.py
    │   ├── inspect_seizure_annotations
    │   ├── inspect_seizure_annotations.py
    │   ├── list_mat_files
    │   ├── list_mat_files.sh
    │   ├── list_multiple_tar_files.log
    │   ├── list_multiple_tar_files.sh
    │   ├── list_n_mat_files.sh
    │   ├── list_tar_file.log
    │   ├── list_tar_file.sh
    │   ├── README.md
    │   ├── remove_mat_files.sh
    │   ├── rsync_from_punim2354_to_punim0264.log
    │   ├── rsync_from_punim2354_to_punim0264.sh
    │   ├── rsync_from_punim2354_to_punim0264.sh%.log
    │   ├── tar_and_notify.log
    │   ├── tar_and_notify.sh
    │   ├── tar_each_dir.sh
    │   ├── tar_patients.sh
    │   ├── tar_patients.sh.tar-process.log
    │   └── titan.md
    ├── db
    │   ├── check_db_sizes.sh
    │   ├── check_duplicates.log
    │   ├── combine_all_dbs.py
    │   ├── delete_duplicates
    │   ├── delete_duplicates.log
    │   ├── extract_timetable
    │   ├── extract_timetable.py
    │   ├── extract_timetables.sh
    │   ├── fixing_scripts
    │   ├── init_as_unverified.log
    │   ├── inspect_dbs
    │   ├── inspect_dbs.log
    │   ├── inspect_mat_file_format.py
    │   ├── load_db_test
    │   ├── ls_db
    │   ├── MigrateDB.py
    │   ├── NeuroVistaDBEEGPopulate.py
    │   ├── NeuroVistaDBEEG.py
    │   ├── NeuroVistaDBPACPopulate.py
    │   ├── NeuroVistaDBPAC.py
    │   ├── NeuroVistaDB.py
    │   ├── old
    │   ├── populate_db
    │   ├── populate_db.log
    │   ├── populate_db.py
    │   ├── populate_db.sh
    │   ├── populate_dbs.log
    │   ├── populate_dbs.sh
    │   ├── populate_meta_db
    │   ├── populate_meta_db.log
    │   ├── populate_meta_db.py
    │   ├── populate_meta_dbs.log
    │   ├── populate_meta_dbs.sh
    │   ├── populate_meta_modules
    │   ├── rsync_dbs_from_titan_to_spartan.sh
    │   ├── softlink_dbs.sh
    │   ├── stop_populating.sh
    │   ├── summarize
    │   ├── summarize_all
    │   ├── symlink_dbs.sh
    │   ├── verify_db
    │   └── verify_db.py
    ├── eda
    │   ├── check_interictal_control
    │   ├── check_interictal_control.py
    │   ├── covariance_matrix
    │   ├── covariance_matrix.py
    │   ├── covariance_matrix.sh
    │   ├── covariance_matrix.sh.log
    │   ├── demographic_data
    │   ├── _for_seerpy_data
    │   ├── __init__.py
    │   ├── inspect_annotations
    │   ├── inspect_dropout
    │   ├── inter_seizure_interval
    │   ├── inter_seizure_interval.py
    │   ├── plot_covariance_matrix.py
    │   ├── plot_covariance_matrix.sh
    │   ├── plot_covariance_matrix.sh.log
    │   ├── plot_seizure_aligned_signals
    │   ├── plot_seizure_aligned_signals.log
    │   ├── plot_seizure_aligned_signals.py
    │   ├── plot_seizure_aligned_signals.sh
    │   ├── plot_seizure_aligned_signals.sh.log
    │   ├── plot_seizure_aligned_signals_working.py
    │   ├── plot_seizure_raster
    │   └── plot_seizure_raster.py
    ├── __init__.py
    ├── io
    │   └── load_db
    ├── ml
    │   ├── catboost
    │   ├── clf
    │   ├── clf_catboost
    │   ├── clf_catboost_on_descriptive_stats
    │   ├── clf_catboost_on_descriptive_stats.py
    │   ├── clf_catboost_on_descriptive_stats.sh
    │   ├── clf_catboost_on_vit_extracted_features
    │   ├── clf_catboost_on_vit_extracted_features.py
    │   ├── clf_catboost_on_vit_extracted_features_sbatch.py
    │   ├── clf_catboost_on_vit_extracted_features_sbatch.sh
    │   ├── clf_catboost_on_vit_extracted_features_timeseries_CV
    │   ├── clf_catboost_on_vit_extracted_features_timeseries_CV.py
    │   ├── clf_catboost_on_vit_extracted_features_timeseries_CV_sbatch.sh
    │   ├── clf_catboost_on_x_cat_results.sh
    │   ├── clf_catboost_on_x_cat_results.sh.csv
    │   ├── clf_vit
    │   ├── clf_vit.py
    │   ├── clf_vit_seerpy_working.py
    │   ├── _for_seerpy_data
    │   ├── helpers
    │   ├── __init__.py
    │   ├── __pycache__
    │   ├── reg_catboost_on_descriptive_stats
    │   ├── reg_catboost_on_descriptive_stats.py
    │   ├── summarize_clf_results.sh
    │   ├── summarize_clf_results.sh.txt
    │   ├── visualize_pac_and_vit_attentions
    │   └── visualize_pac_and_vit_attentions.py
    ├── pac
    │   ├── _arr2vit_frts_sbatch.sh
    │   ├── arr2vit_frts_sbatch.sh
    │   ├── arr2vit_ftrs
    │   ├── arr2vit_ftrs.py
    │   ├── arr2vit_ftrs_sbatch_long.sh
    │   ├── arr2vit_ftrs_sbatch.sh
    │   ├── _calc_pac
    │   ├── calc_pac
    │   ├── calc_pac_around_seizures
    │   ├── calc_pac_around_seizures 
    │   ├── calc_pac_around_seizures.py
    │   ├── calc_pac_around_seizures_sbatch_long.sh
    │   ├── calc_pac_around_seizures_sbatch_long.sh.logs
    │   ├── calc_pac_around_seizures_sbatch.sh
    │   ├── calc_pac_around_seizures_sbatch.sh.logs
    │   ├── _calc_pac.py
    │   ├── _calc_pac_sbatch.logs
    │   ├── calc_pac_sbatch_long.sh
    │   ├── calc_pac_sbatch_long.sh.logs
    │   ├── calc_pac_sbatch.sh
    │   ├── calc_pac_sbatch.sh.logs
    │   ├── count_n_seiizures_in_pac_db
    │   ├── count_n_seiizures_in_pac_db.py
    │   ├── count_n_seizures_in_pac_db
    │   ├── count_n_seizures_in_pac_db.py
    │   ├── db2arr
    │   ├── db2arr.py
    │   ├── describe
    │   ├── describe.py
    │   ├── im2vid_pac_optical_flow.sh
    │   ├── im2vid_pac.sh
    │   ├── plot_descriptive_stats
    │   ├── plot_descriptive_stats.py
    │   ├── pre_seizure_pac_descriptive_statistic_values
    │   ├── pre_seizure_pac_descriptive_statistic_values.py
    │   ├── pre_seizure_pac_descriptive_statistic_values.sh
    │   ├── pre_seizure_pac_descriptive_statistic_values.sh.logs
    │   ├── scale_check.py
    │   ├── visualize_pac
    │   ├── visualize_pac_optical_flow
    │   ├── visualize_pac_optical_flow.py
    │   ├── visualize_pac_optical_flow_sbatch.sh
    │   ├── visualize_pac_optical_flow_sbatch_.sh
    │   ├── visualize_pac.py
    │   ├── visualize_pac_sbatch.logs
    │   ├── visualize_pac_sbatch.sh
    │   └── visualize_pac_sbatch.sh.logs
    ├── postgres
    │   ├── examples
    │   ├── README.md
    │   ├── schema
    │   ├── setup
    │   ├── setup.sh
    │   └── setup.sh.log
    ├── utils
    │   ├── _add_eeg_dtype_and_shape.py
    │   ├── im2vid.sh
    │   ├── __init__.py
    │   ├── list_figures.sh -> .list_figures-versions/list_figures_v001.sh
    │   ├── _load_eeg_around_seizures.py
    │   ├── _load_pac
    │   ├── _load_pac_all
    │   ├── _load_pac_all.py -> ._load_pac_all-versions/_load_pac_all_v001.py
    │   ├── _load_pac.py
    │   ├── _load_seizure_aligned_eeg
    │   ├── _load_seizure_aligned_eeg.py -> ._load_seizure_aligned_eeg-versions/_load_seizure_aligned_eeg_v001.py
    │   ├── merge_figures_pac_amp.py
    │   ├── _parse_lpath_mat.py
    │   ├── __pycache__
    │   └── _rm_figures.sh
    └── vit
        └── pac2vitfeatures.py
```