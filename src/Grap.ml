open Graphics


let dessine_map  = 
    open_graph " 640 x480 " ;
    set_window_title " map " ;
    draw_rect 5 5 20 20; 
    draw_rect 100 100 20 20; 
    read_key ()

;;
