    {"get-temp-file", 0, 0, e_get_temp_file},
    {"add-mode-global", 1, 1, e_add_mode_global},
    {"message", 1, 1, e_message},
    {"log-message", 1, 1, e_log_message},
    {"log-debug", 1, 1, e_log_debug},
    {"insert-string", 1, 1, e_insert_string},
    {"set-point", 1, 1, e_set_point},
    {"get-mark", 0, 0, e_get_mark},
    {"get-point", 0, 0, e_get_point},
    {"get-point-max", 0, 0, e_get_point_max},
    {"set-key", 2, 2, e_set_key},
    {"prompt", 2, 2, e_prompt},
    {"show-prompt", 2, 2, e_show_prompt},
    {"eval-block", 0, 0, e_eval_block},
    {"get-buffer-name", 0, 0, e_get_buffer_name},
    {"get-char", 0, 0, e_get_char},
    {"get-key", 0, 0, e_get_key},
    {"get-key-name", 0, 0, e_get_key_name},
    {"get-key-funcname", 0, 0, e_get_key_funcname},
    {"goto-line", 1, 1, e_goto_line},
    {"getch", 0, 0, e_getch},
    {"get-version-string", 0, 0, e_get_version_string},
    {"save-buffer", 1, 1, e_save_buffer},
    {"search-forward", 1, 1, e_search_forward},
    {"search-backward", 1, 1, e_search_backward},
    {"insert-file-contents-literally", 1, 2, e_insert_file},
    {"select-buffer", 1, 1, e_select_buffer},
    {"rename-buffer", 1, 1, e_rename_buffer},
    {"kill-buffer", 1, 1, e_kill_buffer},
    {"erase-buffer", 0, 0, e_zero_buffer},
    {"find-file", 1, 1, e_find_file},
    {"update-display", 0, 0, e_update_display},
    {"prompt-filename", 1, 1, e_getfilename},
    {"clear-message-line", 0, 0, e_clear_message_line},
    {"refresh", 0, 0, e_refresh},

    {"beginning-of-buffer", 0, 0, e_beginning_of_buffer},
    {"end-of-buffer", 0, 0, e_end_of_buffer},
    {"beginning-of-line", 0, 0, e_lnbegin},
    {"end-of-line", 0, 0, e_lnend},
    {"forward-char", 0, 0, e_right},
    {"forward-page", 0, 0, e_forward_page},
    {"forward-word", 0, 0, e_forward_word},
    {"backward-char", 0, 0, e_left},
    {"backward-page", 0, 0, e_backward_page},
    {"backward-word", 0, 0, e_backward_word},
    {"next-line", 0, 0, e_down},
    {"previous-line", 0, 0, e_up},
    {"set-mark", 0, 0, e_set_mark},
    {"set-clipboard", 1, 1, e_set_clipboard},
    {"delete", 0, 0, e_delete},
    {"copy-region", 0, 0, e_copy_region},
    {"kill-region", 0, 0, e_kill_region},
    {"yank", 0, 0, e_yank},
    {"backspace", 0, 0, e_backspace},
    {"delete-other-windows", 0, 0, e_delete_other_windows},
    {"execute-key", 0, 0, e_execute_key},
    {"list-buffers", 0, 0, e_list_buffers},
    {"describe-bindings", 0, 0, e_describe_bindings},
    {"describe-functions", 0, 0, e_describe_functions},
    {"split-window", 0, 0, e_split_window},
    {"other-window", 0, 0, e_other_window},
    {"get-clipboard", 0, 0, e_get_clipboard},
    {"get-buffer-count", 0, 0, e_get_buffer_count},
    {"suspend", 0, 0, e_suspend},
    {"exit", 0, 0, e_quit}
