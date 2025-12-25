/*
  This file is included in lisp.c

  Here we register editor commands that we want to be
  available to be called as lisp functions

*/

/* Text manipulation: read from, write to buffer text */

{"backspace",                 0, 0, 0,            e_backspace},
{"delete",                    0, 0, 0,            e_delete},
{"erase-buffer",              0, 0, 0,            e_zero_buffer},
{"get-char",                  0, 0, 0,            e_get_char},
{"insert-string",             1, 1, TYPE_STRING,  e_insert_string},
{"kill-region",               0, 0, 0,            e_kill_region},
{"yank",                      0, 0, 0,            e_yank},

/* Selection */
{"copy-region",               0, 0, 0,            e_copy_region},
{"get-clipboard",             0, 0, 0,            e_get_clipboard},
{"get-mark",                  0, 0, 0,            e_get_mark},
{"set-clipboard",             1, 1, TYPE_STRING,  e_set_clipboard},
{"set-mark",                  0, 0, 0,            e_set_mark},

/* Cursor Movement and information */
{"backward-char",             0, 0, 0,            e_left},
{"backward-word",             0, 0, 0,            e_backward_word},
{"beginning-of-buffer",       0, 0, 0,            e_beginning_of_buffer},
{"beginning-of-line",         0, 0, 0,            e_lnbegin},
{"end-of-buffer",             0, 0, 0,            e_end_of_buffer},
{"end-of-line",               0, 0, 0,            e_lnend},
{"forward-char",              0, 0, 0,            e_right},
{"forward-word",              0, 0, 0,            e_forward_word},
{"get-point",                 0, 0, 0,            e_get_point},
{"get-point-max",             0, 0, 0,            e_get_point_max},
{"goto-line",                 1, 1, TYPE_INTEGER, e_goto_line},
{"next-line",                 0, 0, 0,            e_down},
{"previous-line",             0, 0, 0,            e_up},
{"scroll-up",                 0, 0, 0,            e_scroll_up},
{"scroll-down",               0, 0, 0,            e_scroll_down},
{"search-forward",            1, 1, TYPE_STRING,  e_search_forward},
{"search-backward",           1, 1, TYPE_STRING,  e_search_backward},
{"set-point",                 1, 1, TYPE_INTEGER, e_set_point},

/* Buffer Management and information */
{"find-buffer-visiting",      1, 1, TYPE_STRING,  e_find_buffer_by_fname},
{"buffer-filename",           0, 1, TYPE_STRING,  e_get_buffer_filename},
{"buffer-fread",              1, 2, 0,            e_buffer_fread},
{"buffer-fwrite",             1, 2, 0,            e_buffer_fwrite},
{"buffer-mode",               0, 2, 0,            e_buffer_mode},
{"buffer-modified-p",         0, 2, 0,            e_buffer_modified_p},
{"buffer-overwrite-p",        0, 2, 0,            e_buffer_overwrite_p},
{"buffer-readonly-p",         0, 2, 0,            e_buffer_readonly_p},
{"buffer-special-p",          0, 2, 0,            e_buffer_special_p},
{"buffer-undo-p",             0, 2, 0,            e_buffer_undo_p},
{"buffer-next",               0, 1, TYPE_STRING,  e_buffer_next},
{"buffer-show",               1, 1, TYPE_STRING,  e_buffer_show},
//{"current-buffer",            0, 0, 0,            e_current_buffer},
{"delete-buffer",             1, 1, TYPE_STRING,  e_delete_buffer},
{"get-buffer-create",         1, 1, TYPE_STRING,  e_get_buffer_create},
{"list-buffers",              0, 0, 0,            e_list_buffers},
{"set-buffer",                1, 1, TYPE_STRING,  e_set_buffer},
{"set-buffer-name",           1, 1, TYPE_STRING,  e_set_buffer_name},
{"set-visited-filename",      1, 1, 0,            e_set_buffer_filename},

/* Window Handling */
{"delete-other-windows",      0, 0, 0,            e_delete_other_windows},
{"split-window",              0, 0, 0,            e_split_window},
{"other-window",              0, 0, 0,            e_other_window},
{"update-display",            0, 0, 0,            e_update_display},
{"refresh",                   0, 0, 0,            e_refresh},

/* Message Line */
{"message",                   1, 1, TYPE_STRING,  e_message},
{"prompt-filename",           1, 2, TYPE_STRING,  e_prompt_filename},
{"clear-message-line",        0, 0, 0,            e_clear_message_line},
{"prompt",                    1, 2, TYPE_STRING,  e_prompt},

/* Keyboard Handling */
{"describe-bindings",         0, 0, 0,            e_describe_bindings},
{"describe-functions",        0, 0, 0,            e_describe_functions},
{"execute-key",               0, 0, 0,            e_execute_key},
{"getch",                     0, 0, 0,            e_getch},
{"get-key",                   0, 0, 0,            e_get_key},
{"get-key-funcname",          0, 0, 0,            e_get_key_funcname},
{"get-key-name",              0, 0, 0,            e_get_key_name},
{"set-key",                   2, 2, TYPE_STRING,  e_set_key},

/* Programming and System Interaction */
{"suspend",                   0, 0, 0,            e_suspend},
{"exit",                      0, 0, 0,            e_quit},
{"get-temp-file",             0, 0, 0,            e_get_temp_file},
{"log-message",               1, 1, TYPE_STRING,  e_log_message},
{"log-debug",                 1, 1, TYPE_STRING,  e_log_debug},
{"eval-block",                0, 0, 0,            e_eval_block},
{"get-version-string",        0, 0, 0,            e_get_version_string}
