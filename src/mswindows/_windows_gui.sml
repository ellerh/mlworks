(*
 * $Log: _windows_gui.sml,v $
 * Revision 1.9  1998/07/24 08:59:46  johnh
 * [Bug #30438]
 * Implement BitBlt - pass a record.
 *
 * Revision 1.8  1998/07/23  14:19:01  johnh
 * [Bug #30451]
 * Implement SetBkMode and GetBkMode and fix timer text on splash screen.
 *
 * Revision 1.7  1998/07/16  11:46:06  johnh
 * [Bug #30441]
 * Add createWindowEx.
 *
 * Revision 1.6  1998/06/25  15:57:42  johnh
 * [Bug #30431]
 * Extend Capi to allow setting of window attributes.
 *
 * Revision 1.5  1998/06/05  11:44:06  johnh
 * [Bug #30411]
 * Add CenterWindow.
 *
 * Revision 1.4  1998/05/19  15:58:06  johnh
 * [Bug #30369]
 * File dialogs must allow multiple selection.
 *
 * Revision 1.3  1998/04/17  12:19:50  johnh
 * [Bug #30318]
 * Add createDialog to Windows structure.
 *
 * Revision 1.2  1998/04/02  11:47:39  jont
 * Automatic checkin:
 * changed attribute _comment to ' * '
 *
 *
 * Copyright (C) 1995 Harlequin Ltd
 *
 * Revision 1.48  1998/02/19  16:18:06  jont
 * [Bug #70070]
 * Remove MLWorks.IO.terminal_out in favour of Terminal.output
 *
 * Revision 1.47  1998/01/27  15:26:00  johnh
 * [Bug #30071]
 * Merge in Project Workspace changes.
 *
 * Revision 1.46  1997/10/16  14:33:14  johnh
 * [Bug #30059]
 * Implement combo boxes for create dialog.
 *
 * Revision 1.44  1997/10/10  10:54:51  johnh
 * [Bug #30204]
 * Update binding of runtime exception.
 *
 * Revision 1.43.2.3  1997/11/20  16:11:10  johnh
 * [Bug #30071]
 * Generalise openFileDialog to take a description and a mask.
 *
 * Revision 1.43.2.2  1997/09/12  14:48:42  johnh
 * [Bug #30071]
 * Redesign Compilation Manager -> Project Workspace.
 *
 * Revision 1.43  1997/09/05  14:31:03  johnh
 * [Bug #30241]
 * Implementing proper Find Dialog.
 *
 * Revision 1.42  1997/05/19  14:11:57  johnh
 * Implementing toolbar.
 *
 * Revision 1.41  1997/05/16  15:34:46  johnh
 * Implementing single menu bar on Windows.
 *
 * Revision 1.40  1997/05/06  10:27:34  jont
 * [Bug #30088]
 * Get rid of MLWorks.Option
 *
 * Revision 1.39  1997/03/25  17:26:58  johnh
 * [Bug #1992]
 * Added WM_CONTEXTMENU message value.
 *
 * Revision 1.38  1997/03/17  14:22:30  johnh
 * [Bug #1954]
 * Added WM_SIZING value (used by Capi.set_min_window_size).
 *
 * Revision 1.37  1996/11/06  11:18:13  matthew
 * [Bug #1728]
 * __integer becomes __int
 *
 * Revision 1.36  1996/10/25  15:45:59  johnh
 * [Bug #1687]
 * Removed redundant isAltOn function.
 *
 * Revision 1.35  1996/09/30  13:43:05  johnh
 * Added scrolling functionality.
 *
 * Revision 1.33  1996/09/25  15:29:21  johnh
 * [Bug #1613]
 * [Bug #1613]
 * Added get_scroll_info and set_pixel and also added a check to ensure that.
 * the WindowSystemError is not redefined on compiling Windows.sml in Gui.
 *
 * Revision 1.32  1996/07/26  14:59:10  daveb
 * [Bug #1478]
 * Added WM_USER[0-5] messages.
 *
 * Revision 1.31  1996/06/18  13:23:08  daveb
 * Moved exception WindowSystemError here from _capi.
 * Added DEFAULT_GUI_FONT to stock_object datatype.
 * Added WM_INITDIALOG to message datatype.
 *
 * Revision 1.30  1996/06/13  11:24:53  daveb
 * Added WM_SYSCOMMAND, sc_value and convertScValue, and SW_RESTORE etc.
 *
 * Revision 1.29  1996/05/28  16:09:21  jont
 * add saveImageDialog
 *
 * Revision 1.28  1996/05/23  09:04:46  matthew
 * Changed type of word shift operations
 *
 * Revision 1.27  1996/05/13  16:33:12  matthew
 * Changes to basis
 *
 * Revision 1.26  1996/05/01  12:15:33  jont
 * String functions explode, implode, chr and ord now only available from String
 * io functions and types
 * instream, oustream, open_in, open_out, close_in, close_out, input, output and end_of_stream
 * now only available from MLWorks.IO
 *
 * Revision 1.25  1996/04/30  13:24:54  matthew
 * Removing MLWorks.Integer
 *
 * Revision 1.24  1996/04/18  15:22:11  jont
 * initbasis moves to basis
 *
 * Revision 1.23  1996/04/10  10:48:24  matthew
 * New language definition
 *
 * Revision 1.22  1996/03/20  14:26:09  matthew
 * Changes for new language definition
 *
 * Revision 1.21  1996/03/07  14:16:05  matthew
 * Extending
 *
 * Revision 1.20  1996/03/01  11:25:34  matthew
 * Extending library functions
 *
 * Revision 1.19  1996/02/27  16:09:26  matthew
 * More rationalization
 *
 * Revision 1.18  1996/02/02  15:03:47  matthew
 * Extending library functions
 *
 * Revision 1.17  1996/01/25  12:27:09  matthew
 * Adding get_bg_color etc.
 *
 * Revision 1.16  1996/01/12  16:35:41  matthew
 * Adding WM_SETREDRAW message
 *
 * Revision 1.15  1996/01/12  10:00:34  daveb
 * Added open_file_dialog, open_dir_dialog and save_as_dialog.
 *
 * Revision 1.14  1996/01/04  16:16:47  matthew
 * Adding get_stock_object
 *
 * Revision 1.13  1995/12/20  15:14:02  matthew
 * Adding color functions
 *
 * Revision 1.12  1995/12/15  14:31:48  matthew
 * Adding new window styles.
 *
 * Revision 1.11  1995/12/14  14:22:37  matthew
 * Changing message handling
 *
 * Revision 1.10  1995/12/06  17:16:30  matthew
 * Adding clipboard functionality
 *
 * Revision 1.9  1995/11/21  11:11:19  matthew
 * More stuff
 *
 * Revision 1.8  1995/11/14  13:57:53  matthew
 * Extending for graphics
 *
 * Revision 1.7  1995/09/19  14:02:27  matthew
 * Changes for new word signature
 *
 * Revision 1.6  1995/09/05  10:50:15  matthew
 * Adding word_to_signed_int
 *
 * Revision 1.5  1995/08/31  10:12:16  matthew
 * Adding extra menu functions
 *
 * Revision 1.4  1995/08/25  10:18:35  matthew
 * More stuff
 *
 * Revision 1.3  1995/08/15  11:23:45  matthew
 * Extending
 *
 * Revision 1.2  1995/08/11  08:36:44  matthew
 * Making it all work
 *
 * Revision 1.1  1995/08/03  12:53:13  matthew
 * new unit
 * MS Windows GUI
 *
 *)

require "../basis/__int";

require "^.utils.__terminal";
require "windows_gui";
require "../basis/__word32";

functor WindowsGui () : WINDOWS_GUI =
  struct
    structure Word = Word32

    exception WindowSystemError of string


    val print = Terminal.output

    fun env s = MLWorks.Internal.Value.cast (MLWorks.Internal.Runtime.environment s)
    exception Unimplemented of string
    fun unimpl s = fn _ => (print (s ^ " unimplemented\n"); raise Unimplemented s)

    local 
      val windows_exns_initialised = env "nt windows exns initialised"
      val WindowSystemErrorExn = env "exception win"
    in
      val _ = 
	if !windows_exns_initialised then 
	    MLWorks.Internal.Value.update_exn (WindowSystemError "", WindowSystemErrorExn)
	  else
	    (windows_exns_initialised := true;
	     WindowSystemErrorExn := WindowSystemError "")
    end

    val N = Int.toString

    type word = Word.word
    val intToWord = Word.fromInt
    val wordToInt = Word.toInt

    val nullWord = intToWord 0

    (* There should be a better way of doing this *)
    fun wordToSignedInt w =
      wordToInt w
      handle _ => ~(wordToInt (Word.- (nullWord,w)))

    fun W w = N (wordToInt w) (* no Word.makestring! *)

    datatype hwnd = HWND of word
    fun windowToWord (HWND w) = w
    fun wordToWindow w = HWND w

    datatype hmenu = HMENU of word
    fun menuToWord (HMENU w) = w
    fun wordToMenu w = HMENU w

    datatype accelerator_table = ACCELERATOR_TABLE of word
    datatype wparam = WPARAM of word
    datatype lparam = LPARAM of word
    datatype hdc = HDC of word
    datatype timer = TIMER of word
    datatype color = COLOR of word
    datatype cursor = CURSOR of word
    datatype hinst = HINSTANCE of word

    val nullWindow = HWND nullWord
    val null_menu = HMENU nullWord

    fun isNullWindow (hwnd) = hwnd = nullWindow

    datatype rect = RECT of {left:int,top:int,right:int,bottom:int}
    datatype point = POINT of {x:int,y:int}

    (* It would be nice to auto generate this lot *)
    datatype message =
      BM_GETCHECK |
      BM_GETSTATE |
      BM_SETCHECK |
      BM_SETSTATE |
      BM_SETSTYLE |
      
      BN_CLICKED |
      BN_DISABLE |
      BN_DOUBLECLICKED |
      BN_HILITE |
      BN_PAINT |
      BN_UNHILITE |
      
      CBN_CLOSEUP |
      CBN_DBLCLK |
      CBN_DROPDOWN |
      CBN_EDITCHANGE |
      CBN_EDITUPDATE |
      CBN_ERRSPACE |
      CBN_KILLFOCUS |
      CBN_SELCHANGE |
      CBN_SELENDCANCEL |
      CBN_SELENDOK |
      CBN_SETFOCUS |

      CB_ADDSTRING |
      CB_DELETESTRING |
      CB_DIR |
      CB_FINDSTRING |
      CB_FINDSTRINGEXACT |
      CB_GETCOUNT |
      CB_GETCURSEL |
      CB_GETDROPPEDCONTROLRECT |
      CB_GETDROPPEDSTATE |
      CB_GETDROPPEDWIDTH |
      CB_GETEDITSEL |
      CB_GETEXTENDEDUI |
      CB_GETHORIZONTALEXTENT |
      CB_GETITEMDATA |
      CB_GETITEMHEIGHT |
      CB_GETLBTEXT |
      CB_GETLBTEXTLEN |
      CB_GETLOCALE |
      CB_GETTOPINDEX |
      CB_INITSTORAGE |
      CB_INSERTSTRING |
      CB_LIMITTEXT |
      CB_RESETCONTENT |
      CB_SELECTSTRING |
      CB_SETCURSEL |
      CB_SETDROPPEDWIDTH |
      CB_SETEDITSEL |
      CB_SETEXTENDEDUI |
      CB_SETHORIZONTALEXTENT |
      CB_SETITEMDATA |
      CB_SETITEMHEIGHT |
      CB_SETLOCALE |
      CB_SETTOPINDEX |
      CB_SHOWDROPDOWN |

      DM_GETDEFID |
      DM_SETDEFID |
      
      EM_CANUNDO |
      EM_EMPTYUNDOBUFFER |
      EM_FMTLINES |
      EM_GETFIRSTVISIBLELINE |
      EM_GETHANDLE |
      EM_GETLINE |
      EM_GETLINECOUNT |
      EM_GETMODIFY |
      EM_GETPASSWORDCHAR |
      EM_GETRECT |
      EM_GETSEL |
      EM_GETWORDBREAKPROC |
      EM_LIMITTEXT |
      EM_LINEFROMCHAR |
      EM_LINEINDEX |
      EM_LINELENGTH |
      EM_LINESCROLL |
      EM_REPLACESEL |
      EM_SCROLL |
      EM_SCROLLCARET |
      EM_SETHANDLE |
      EM_SETMODIFY |
      EM_SETPASSWORDCHAR |
      EM_SETREADONLY |
      EM_SETRECT |
      EM_SETRECTNP |
      EM_SETSEL |
      EM_SETTABSTOPS |
      EM_SETWORDBREAKPROC |
      EM_UNDO |
      EN_CHANGE |
      EN_ERRSPACE |
      EN_HSCROLL |
      EN_KILLFOCUS |
      EN_MAXTEXT |
      EN_SETFOCUS |
      EN_UPDATE |
      EN_VSCROLL |

      FINDMSGSTRING |

      LBN_DBLCLK |
      LBN_ERRSPACE |
      LBN_KILLFOCUS |
      LBN_SELCANCEL |
      LBN_SELCHANGE |
      LBN_SETFOCUS |
      
      LB_ADDFILE |
      LB_ADDSTRING |
      LB_DELETESTRING |
      LB_DIR |
      LB_FINDSTRING |
      LB_FINDSTRINGEXACT |
      LB_GETANCHORINDEX |
      LB_GETCARETINDEX |
      LB_GETCOUNT |
      LB_GETCURSEL |
      LB_GETHORIZONTALEXTENT |
      LB_GETITEMDATA |
      LB_GETITEMHEIGHT |
      LB_GETITEMRECT |
      LB_GETLOCALE |
      LB_GETSEL |
      LB_GETSELCOUNT |
      LB_GETSELITEMS |
      LB_GETTEXT |
      LB_GETTEXTLEN |
      LB_GETTOPINDEX |
      LB_INSERTSTRING |
      LB_RESETCONTENT |
      LB_SELECTSTRING |
      LB_SELITEMRANGE |
      LB_SELITEMRANGEEX |
      LB_SETANCHORINDEX |
      LB_SETCARETINDEX |
      LB_SETCOLUMNWIDTH |
      LB_SETCOUNT |
      LB_SETCURSEL |
      LB_SETHORIZONTALEXTENT |
      LB_SETITEMDATA |
      LB_SETITEMHEIGHT |
      LB_SETLOCALE |
      LB_SETSEL |
      LB_SETTABSTOPS |
      LB_SETTOPINDEX |
      
      TB_GETSTATE |
      TB_SETSTATE |

      WM_ACTIVATE |
      WM_ACTIVATEAPP |
      WM_CANCELMODE |
      WM_CHAR |
      WM_CHARTOITEM |
      WM_CHILDACTIVATE |
      WM_CLOSE |
      WM_COMMAND |
      WM_CONTEXTMENU |
      WM_COPY |
      WM_COPYDATA |
      WM_CREATE |
      WM_CTLCOLORBTN |
      WM_CTLCOLOREDIT |
      WM_CUT |
      WM_DEADCHAR |
      WM_DESTROY |
      WM_ENABLE |
      WM_ENDSESSION |
      WM_ERASEBKGND |
      WM_GETFONT |
      WM_GETMINMAXINFO |
      WM_GETTEXT |
      WM_GETTEXTLENGTH |
      WM_HOTKEY |
      WM_HSCROLL |
      WM_INITDIALOG |
      WM_INITMENU |
      WM_KEYDOWN |
      WM_KEYUP |
      WM_KILLFOCUS |
      WM_LBUTTONDBLCLK |
      WM_LBUTTONDOWN |
      WM_LBUTTONUP |
      WM_MBUTTONDBLCLK |
      WM_MBUTTONDOWN |
      WM_MBUTTONUP |
      WM_MOUSEACTIVATE |
      WM_MOUSEMOVE |
      WM_MOVE |
      WM_NCACTIVATE |
      WM_NCCALCSIZE |
      WM_NCCREATE |
      WM_NCDESTROY |
      WM_NCHITTEST |
      WM_NCLBUTTONDBLCLK |
      WM_NCLBUTTONDOWN |
      WM_NCLBUTTONUP |
      WM_NCMBUTTONDBLCLK |
      WM_NCMBUTTONDOWN |
      WM_NCMBUTTONUP |
      WM_NCMOUSEMOVE |
      WM_NCRBUTTONDBLCLK |
      WM_NCRBUTTONDOWN |
      WM_NCRBUTTONUP |
      WM_NOTIFY |
      (* WM_OPENICON | *)
      WM_PAINT |
      WM_PARENTNOTIFY |
      WM_PASTE |
      WM_POWER |
      WM_QUERYENDSESSION |
      WM_QUERYOPEN |
      WM_QUEUESYNC |
      WM_QUIT |
      WM_RBUTTONDBLCLK |
      WM_RBUTTONDOWN |
      WM_RBUTTONUP |
      WM_SETCURSOR | 
      WM_SETFOCUS |
      WM_SETFONT |
      WM_SETREDRAW |
      WM_SETTEXT |
      WM_SHOWWINDOW |
      WM_SIZE |
      WM_SIZING |
      WM_SYSCHAR |
      WM_SYSCOMMAND |
      WM_SYSDEADCHAR |
      WM_SYSKEYDOWN |
      WM_SYSKEYUP |
      WM_UNDO |		
      WM_USER0 |
      WM_USER1 |
      WM_USER2 |
      WM_USER3 |
      WM_USER4 |
      WM_USER5 |
      WM_VSCROLL |
      WM_WINDOWPOSCHANGED |
      WM_WINDOWPOSCHANGING 
      (* WN_DELETEITEM | *)
      (* WN_VKEYTOITEM *)



    datatype window_style =
      BS_3STATE |
      BS_AUTO3STATE |
      BS_AUTOCHECKBOX |
      BS_AUTORADIOBUTTON |
      BS_CHECKBOX |
      BS_DEFPUSHBUTTON |
      BS_GROUPBOX |
      BS_LEFTTEXT |
      BS_OWNERDRAW |
      BS_PUSHBUTTON |
      BS_RADIOBUTTON |
      BS_USERBUTTON |

      CBS_AUTOHSCROLL |
      CBS_DISABLENOSCROLL |
      CBS_DROPDOWN |
      CBS_DROPDOWNLIST |
      CBS_HASSTRINGS |
      CBS_NOINTEGRALHEIGHT |
      CBS_OEMCONVERT |
      CBS_OWNERDRAWFIXED |
      CBS_OWNERDRAWVARIABLE |
      CBS_SIMPLE |
      CBS_SORT |

      DS_ABSALIGN |
      DS_LOCALEDIT |
      DS_MODALFRAME |
      DS_NOIDLEMSG |
      DS_SETFONT |
      DS_SETFOREGROUND |
      DS_SYSMODAL |

      ES_AUTOHSCROLL |
      ES_AUTOVSCROLL |
      ES_CENTER |
      ES_LEFT |
      ES_LOWERCASE |
      ES_MULTILINE |
      ES_NOHIDESEL |
      ES_OEMCONVERT |
      ES_PASSWORD |
      ES_READONLY |
      ES_RIGHT |
      ES_UPPERCASE |
      ES_WANTRETURN |

      LBS_DISABLENOSCROLL |
      LBS_EXTENDEDSEL |
      LBS_HASSTRINGS |
      LBS_MULTICOLUMN |
      LBS_MULTIPLESEL |
      LBS_NODATA |
      LBS_NOINTEGRALHEIGHT |
      LBS_NOREDRAW |
      LBS_NOTIFY |
      LBS_OWNERDRAWFIXED |
      LBS_OWNERDRAWVARIABLE |
      LBS_SORT |
      LBS_STANDARD |
      LBS_USETABSTOPS |
      LBS_WANTKEYBOARDINPUT |

      SBS_BOTTOMALIGN |
      SBS_HORZ |
      SBS_LEFTALIGN |
      SBS_RIGHTALIGN |
      SBS_SIZEBOX |
      SBS_SIZEBOXBOTTOMRIGHTALIGN |
      SBS_SIZEBOXTOPLEFTALIGN |
      SBS_TOPALIGN |
      SBS_VERT |

      SS_BLACKFRAME |
      SS_BLACKRECT |
      SS_CENTER |
      SS_GRAYFRAME |
      SS_GRAYRECT |
      SS_ICON |
      SS_LEFT |
      SS_LEFTNOWORDWRAP |
      SS_NOPREFIX |
      SS_RIGHT |
      SS_SIMPLE |
      SS_WHITEFRAME |
      SS_WHITERECT |

      TBSTYLE_ALTDRAG  |
      TBSTYLE_TOOLTIPS |
      TBSTYLE_WRAPABLE |

      WS_BORDER |
      WS_CAPTION |
      WS_CHILD |
      WS_CLIPCHILDREN |
      WS_CLIPSIBLINGS |
      WS_DISABLED |
      WS_DLGFRAME |
      WS_GROUP |
      WS_HSCROLL |
      WS_ICONIC |
      WS_MAXIMIZE |
      WS_MAXIMIZEBOX |
      WS_MINIMIZE |
      WS_MINIMIZEBOX |
      WS_OVERLAPPED |
      WS_OVERLAPPED_WINDOW |
      WS_POPUP |
      WS_POPUPWINDOW |
      WS_SYSMENU |
      WS_TABSTOP |
      WS_THICKFRAME |
      WS_TILEDWINDOW |
      WS_VISIBLE |
      WS_VSCROLL

    type message_handler = (wparam * lparam -> word option)

    (* This is the type of object stored in the handler map *)
    type handler_map_entry = 
      (hwnd * (* The window *)
       (word * (* The C function to call if not handled by ML *)
        (word * message_handler list ref) list ref)) (* An updatable list of message handlers *)

    val getHandlerMap : unit -> handler_map_entry list = env "nt get handler map"
    val setHandlerMap : handler_map_entry list -> unit = env "nt set handler map"

    fun addNewWindow (window,cproc) =
      let
        val hmap = getHandlerMap ()
      in
        setHandlerMap ((window,(cproc,ref nil)) :: hmap)
      end

    val mainLoop : unit -> unit = env "nt main loop"
    val doInput : unit -> bool = env "nt do input"

    val mainInit : unit -> hwnd = 
      env "nt main init"

    datatype res_type = 
      RT_ACCELERATOR |
      RT_ANICURSOR |
      RT_ANIICON |
      RT_BITMAP |
      RT_CURSOR |
      RT_DIALOG |
      RT_FONT |
      RT_FONTDIR |
      RT_GROUP_CURSOR |
      RT_GROUP_ICON |
      RT_ICON |
      RT_MENU |
      RT_MESSAGETABLE |
      RT_RCDATA |
      RT_STRING |
      RT_VERSION

    val findResource : hinst * string * res_type -> word = 
      env "win32 find resource"
    val lockResource : word -> unit = env "win32 lock resource"
    val loadResource : hinst * word -> word = env "win32 load resource"

    val getModuleHandle : string -> hinst = env "win32 get module handle"
    val loadLibrary : string -> hinst = env "win32 load library"
    val freeLibrary : hinst -> bool = env "win32 free library"


    datatype sw_arg =
      SW_HIDE |
      SW_MAXIMIZE |
      SW_MINIMIZE |
      SW_RESTORE |
      SW_SHOW |
      SW_SHOWDEFAULT |
      SW_SHOWMAXIMIZED |
      SW_SHOWMINIMIZED | 
      SW_SHOWMINNOACTIVE |
      SW_SHOWNA |
      SW_SHOWNOACTIVE |
      SW_SHOWNORMAL

    datatype gw_arg =
      GW_CHILD |
      GW_HWNDFIRST |
      GW_HWNDLAST |
      GW_HWNDNEXT |
      GW_HWNDPREV |
      GW_OWNER

    datatype wa_value = 
      WA_ACTIVE |
      WA_CLICKACTIVE |
      WA_INACTIVE

    val convertWaValue : wa_value -> int = env "win32 ml convert wa value"
    val convertStyle : window_style -> word = env "win32 convert window style"

    (* Ch. 1 Windows *)
    val anyPopup : unit -> bool = env "nt any popup"
    val bringWindowToTop : hwnd -> unit = env "nt bring window to top"
    val centerWindow : hwnd * hwnd -> unit = env "win32 center window"
    val childWindowFromPoint : hwnd * (int * int) -> hwnd = env"nt child window from point"
    val closeWindow : hwnd -> unit = env "nt close window"
    val createWindow : 
      {class: string,
       name: string,
       styles : window_style list,
       width : int,
       height : int,
       parent: hwnd,
       menu : word} -> 
      hwnd = env "nt create window"

    datatype window_ex_style = 
      WS_EX_DLGMODALFRAME |
      WS_EX_STATICEDGE |
      WS_EX_WINDOWEDGE

    val createWindowEx :
      {ex_styles: window_ex_style list,
       class: string,
       name: string,
       x: int,
       y: int,
       width : int,
       height : int,
       parent: hwnd,
       menu : word,
       styles : window_style list} -> 
      hwnd = env "win32 create window ex"

    val destroyWindow : hwnd -> unit = env "nt destroy window"
    val enumChildWindows : hwnd * (hwnd -> unit) -> unit = env "nt enum child windows"
    val enumWindows : (hwnd -> unit) -> unit = env "nt enum windows"
    val findWindow : string * string -> hwnd = env "nt find window"
    val getClientRect : hwnd -> rect = env "nt get client rect";
    val getDesktopWindow : unit-> hwnd = env "nt get desktop window"
    val getForegroundWindow : unit-> hwnd = env "nt get foreground window"
    val getLastActivePopup : hwnd -> hwnd = env "nt get last active popup"
    val getNextWindow : hwnd * gw_arg -> hwnd = env "nt get next window"
    val getParent : hwnd -> hwnd = env "nt get parent"
    val getTopWindow : hwnd -> hwnd = env "nt get top window"
    val getWindow : hwnd * gw_arg -> hwnd = env "nt get window"
    val getWindowRect : hwnd -> rect = env "nt get window rect"
    val getWindowPlacement : hwnd -> int * point * point * rect = 
        env "win32 get window placement"
    val isChild : hwnd * hwnd -> bool = env "nt is child"
    val isIconic : hwnd -> bool = env "nt is iconic"
    val isWindow : hwnd -> bool = env "nt is window"
    val isWindowUnicode : hwnd -> bool = env "nt is window unicode"
    val isWindowVisible : hwnd -> bool = env "nt is window visible"
    val isZoomed : hwnd -> bool = env "nt is zoomed"
    val moveWindow : hwnd * int * int * int * int * bool -> unit = env "nt move window"
    val setForegroundWindow : hwnd -> unit = env "nt set foreground window"
    val setParent : hwnd * hwnd -> hwnd = env "nt set parent"
    val setWindowText : hwnd * string -> unit = env "nt set window text"
    val setWindowPos : hwnd * {x: int, y: int, height: int, width: int} -> unit = 
	env "win32 set window pos"
    val showOwnedPopups : hwnd * bool -> unit = env "nt show owned popups"
    val showWindow : hwnd * sw_arg -> unit = env "nt show window"
    val updateWindow : hwnd -> unit = env "nt update window"
    val windowFromPoint : int * int -> hwnd = env "nt window from point"

    val getMinMaxInfo : word -> point * point * point * point = 
        env "win32 get minmax info"
    val setMinMaxInfo : word * point * point * point * point -> word = 
        env "win32 set minmax info"

    (* Ch 2 Messages *)

    val getInputState : unit -> bool = env "nt get input state"
    val getMessagePos : unit -> int * int = env "nt get message pos"
    val getMessageTime : unit -> int = env "nt get message time"
    val inSendMessage : unit -> bool = env "nt in send message"
    val postMessage : hwnd * message * wparam * lparam -> unit = env "nt post message"
    val postQuitMessage : int -> unit = env "nt post quit message"
    val sendMessage : hwnd * message * wparam * lparam -> word = env "nt send message"
    val messageToWord : message -> word = env "nt convert message"

    (* Ch 3. Window Classes *)

    datatype gwl_value =
      DWL_DLGPROC |
      DWL_MSGRESULT |
      DWL_USER |
      GWL_EXSTYLE |
      GWL_HINSTANCE |
      GWL_HWNDPARENT | 
      GWL_ID | 
      GWL_STYLE |
      GWL_USERDATA |
      GWL_WNDPROC

    val get_window_long_internal : hwnd * int -> word = env "nt get window long"
    val set_window_long_internal : hwnd * int * word -> word = env "nt set window long"
    val convertGwlValue : gwl_value -> int = env "nt convert gwl value"

    fun getWindowLong (hwnd,value) =
      get_window_long_internal (hwnd,convertGwlValue value)

    fun setWindowLong (hwnd,value,x) =
      set_window_long_internal (hwnd,convertGwlValue value,x)

    (* Ch 5. Keyboard Input *)
    val enableWindow : hwnd * bool -> bool = env "nt enable window"
    val getActiveWindow  : unit -> hwnd = env "nt get active window"
    val getFocus : unit -> hwnd = env "nt get focus"
    val isWindowEnabled : hwnd -> bool = env "nt is window enabled"
    val setActiveWindow  : hwnd -> hwnd = env "nt set active window"
    val setFocus : hwnd -> hwnd = env "nt set focus"

    (* Ch 6. Mouse input *)
    val getCapture : unit -> hwnd = env "nt get capture"
    val releaseCapture : unit -> unit = env "nt release capture"
    val setCapture : hwnd -> hwnd = env "nt set capture"

    (* Ch 7. Timers *)
    val killTimer : hwnd * timer -> unit = env "nt kill timer"
    val setTimer : hwnd * int * (unit -> unit) -> timer = env "nt set timer"
      
    (* Ch. 39 Coordinate Spaces & Transformations *)
    val clientToScreen : hwnd * point -> point = env "nt client to screen";
    val screenToClient : hwnd * point -> point = env "nt screen to client";

    (* ML window procedures *)

    fun getWindowEntry window =
      let
        val hmap = getHandlerMap ()
        fun lookup (window,[]) =
          (* Can't find it so ... *)
          let
            val entry = (nullWord,ref [])
          in
            setHandlerMap ((window,entry) :: hmap);
            entry
          end
          | lookup (window,(w,entry) :: rest) =
            if w = window then entry
            else lookup (window,rest)
      in
        lookup (window,hmap)
      end

    fun addMessageHandler (window,message,handler) =
      let
        val (cproc,handlerlistref) = getWindowEntry window
        val message_word = messageToWord message
        fun find_message (m,[]) = NONE
          | find_message (m,(m',r)::rest) =
            if m = m' then SOME r else find_message (m,rest)
      in 
        case find_message (message_word,!handlerlistref) of
          SOME r =>
            r := handler :: !r
          | _ => 
              handlerlistref := (message_word,ref [handler]) :: !handlerlistref
      end

    fun removeWindow (window) =
      let
        val hmap = getHandlerMap ()
        fun remove (w,[],acc) = rev acc
          | remove (w,(entry as (w',_)) :: rest,acc) =
            if w = w' then rev acc @ rest
            else remove (w,rest,entry :: acc)
      in
        setHandlerMap (remove (window,hmap,[]))
      end

    (* Old stuff *)

    val getMlWindowProc : unit -> word = env "nt get ml window proc"

    (* Menus *)
    (* These should be representable by 16 bit values *)
    (* So we could probably use an int for them *)
    val id_count = ref 1000 (* Start at 1000 for no very good reason *)
    fun newControlId () =
      let
        val result = intToWord (!id_count)
      in
        id_count := !id_count+1;
        result
      end

    datatype menu_value = SUBMENU of hmenu | ITEM of word

    datatype menu_flag =
      MF_BITMAP |
      MF_CHECKED |
      MF_BYCOMMAND |
      MF_BYPOSITION |
      MF_DISABLED |
      MF_ENABLED |
      MF_GRAYED |
      MF_MENUBARBREAK |
      MF_MENUBREAK |
      MF_OWNERDRAW |
      MF_POPUP |
      MF_SEPARATOR |
      MF_STRING |
      MF_UNCHECKED

    val append_menu_ : hmenu * menu_flag list * word * string -> unit = env "nt append menu"

    val appendMenu : hmenu * menu_flag list * menu_value * string -> unit = 
      fn (menu,flags,value,string) =>
        case value of 
          SUBMENU (HMENU b) => append_menu_ (menu,flags,b,string)
        | ITEM b => append_menu_ (menu,flags,b,string)

    val checkMenuItem : hmenu * word * menu_flag list -> unit = env "nt check menu item"
    val createMenu : unit -> hmenu = env "nt create menu"
    val createPopupMenu : unit -> hmenu = env "nt create popup menu"
    val destroyMenu : hmenu -> unit = env "nt destroy menu"
    val deleteMenu : hmenu * word * menu_flag -> unit = env "nt remove menu"
    val drawMenuBar : hwnd -> unit = env "nt draw menu bar"
    val enableMenuItem : hmenu * word * menu_flag list -> unit = env "nt enable menu item"
    val getMenu : hwnd -> hmenu = env "nt get menu"
    val getMenuItemId : hmenu * int -> word = env "nt get menu item id"
    val getMenuItemCount : hmenu -> int = env "nt get menu item count"
    val getMenuState : hmenu * word * menu_flag -> menu_flag list = unimpl "nt get menu state"
    val getMenuString : hmenu * word * menu_flag -> string = unimpl "nt get menu string"
    val getSubmenu : hmenu * int -> hmenu = env "nt get submenu"
    val getSystemMenu : hwnd * bool -> hmenu = env "nt get system menu"
    val setMenu : hwnd * hmenu -> unit = env "nt set menu"
    val removeMenu : hmenu * word * menu_flag -> unit = env "nt remove menu"

    (* End libraries *)
    val addCommandHandlerInternal : hwnd * word * (hwnd * int-> unit) -> unit = 
      env "nt add menu command"

    fun addCommandHandler (window,w,handler) =
      addCommandHandlerInternal (window, w, handler)

    (* DIALOGS *)

    val registerPopupWindow : hwnd -> unit = env "nt register popup window"
    val unregisterPopupWindow : hwnd -> unit = env "nt unregister popup window"

    datatype message_box_style =
      MB_ABORTRETRYIGNORE |
      MB_APPLMODAL |
      MB_ICONASTERISK |
      MB_ICONEXCLAMATION |
      MB_ICONHAND |
      MB_ICONINFORMATION |
      MB_ICONQUESTION |
      MB_ICONSTOP |
      MB_OK |
      MB_OKCANCEL |
      MB_RETRYCANCEL |
      MB_YESNO |
      MB_YESNOCANCEL

    val messageBox : hwnd * string * string * message_box_style list -> int = env "nt message box"
    val messageBeep : message_box_style -> unit = env "nt message beep"
      
    val endDialog : hwnd * int -> unit = env "nt end dialog"
    val getDlgItem : hwnd * word -> hwnd = env "nt get dlg item"
    val getDlgCtrlID : hwnd -> int = env "win32 get dlg ctrl id"

    val getDialogBaseUnits : unit -> word = env "nt get dialog base units"

    val getFindFlags : word -> {searchStr: string,
             searchDown: bool,
             matchCase: bool,
             wholeWord: bool,
             findNext: bool,
	     closing: bool} = env "win32 get find flags"
    val findDialog : hwnd * string * bool option * bool option * bool option -> hwnd = 
	env "win32 find dialog"

    val openFileDialog : hwnd * string * string * bool -> string list = 
      env "nt open file dialog"
    val openDirDialog : hwnd -> string = env "nt open dir dialog"
    val saveDialog : hwnd * string * string -> string = env "win32 save dialog"

    val createDialog : hinst * hwnd * string -> hwnd = env "nt create dialog"


    (* CURSORS *)

    datatype cursor_shape = 
      IDC_APPSTARTING |
      IDC_ARROW |
      IDC_CROSS |
      IDC_IBEAM |
      IDC_ICON |
      IDC_NO |
      IDC_SIZE |
      IDC_SIZEALL |
      IDC_SIZENESW |
      IDC_SIZENS |
      IDC_SIZEWE |
      IDC_UPARROW |
      IDC_WAIT

    val clipCursor : rect option -> unit = env "nt clip cursor"
    val getClipCursor : unit -> rect = env "nt get clip cursor"
    val getCursorPos : unit -> point = env "nt get cursor pos"
    val setCursorPos : int * int -> unit = env "nt set cursor pos"
    val showCursor : bool -> int = env "nt show cursor"
    val loadCursor : cursor_shape -> cursor = env "win32 load cursor"
    val setCursor : cursor -> cursor = env "win32 set cursor"

    (* CONTROLS *)
 (*   val createStatusWindow: word * string * hwnd * int -> hwnd = env "nt create status window" *)

    (* BUTTON CONTROLS *)
    val checkDlgButton: hwnd * word * int -> unit = env "nt check dlg button"
    val checkRadioButton: hwnd * word * word * word -> unit = env "nt check radio button"
    val isDlgButtonChecked: hwnd * word -> int = env "nt is dlg button checked"

    (* SCROLLBARS *)

    datatype sb_value =
      SB_BOTH |
      SB_BOTTOM |
      SB_CTL |
      SB_ENDSCROLL |
      SB_HORZ |
      SB_LINEDOWN |
      SB_LINELEFT |
      SB_LINERIGHT |
      SB_LINEUP |
      SB_PAGEDOWN |
      SB_PAGELEFT |
      SB_PAGERIGHT |
      SB_PAGEUP |
      SB_THUMBPOSITION |
      SB_THUMBTRACK |
      SB_TOP |
      SB_VERT

    val convertSbValue : sb_value -> int = env "nt ml convert sb value"

    datatype esb_value =
      ESB_DISABLE_BOTH
    | ESB_DISABLE_DOWN
    | ESB_DISABLE_LEFT
    | ESB_DISABLE_LTUP
    | ESB_DISABLE_RIGHT
    | ESB_DISABLE_RTDN
    | ESB_DISABLE_UP
    | ESB_ENABLE_BOTH

    val enableScrollBar : hwnd * sb_value * esb_value -> unit = env "nt enable scroll bar"
    val setScrollRange : hwnd * sb_value * int * int * bool -> unit = env "nt set scroll range"
    val getScrollRange : hwnd * sb_value -> int * int = env "nt get scroll range"
    val setScrollPos : hwnd * sb_value * int * bool -> unit = env "nt set scroll pos"
    val getScrollPos : hwnd * sb_value -> int = env "nt get scroll pos"
    val showScrollBar : hwnd * sb_value * bool -> unit = env "nt show scroll bar"
    val getScrollInfo : hwnd * sb_value -> int * word * word * int * int * word * int * int = env "nt get scroll info"  

    datatype sc_value =
      SC_CLOSE
    | SC_CONTEXTHELP
    | SC_DEFAULT
    | SC_HOTKEY
    | SC_HSCROLL
    | SC_KEYMENU
    | SC_MAXIMIZE
    | SC_MINIMIZE
    | SC_MOUSEMENU
    | SC_MOVE
    | SC_NEXTWINDOW
    | SC_PREVWINDOW
    | SC_RESTORE
    | SC_SCREENSAVE
    | SC_SIZE
    | SC_TASKLIST
    | SC_VSCROLL

    val convertScValue : sc_value -> int = env "nt ml convert sc value"

    (* GRAPHICS *)

    datatype object = OBJECT of word
    datatype hbrush = HBRUSH of word
    datatype hpen = HPEN of word

    datatype object_type = 
      OBJ_PEN | 
      OBJ_BRUSH |
      OBJ_PAL |
      OBJ_FONT |
      OBJ_BITMAP

    (* get_stock_object etc *)
    datatype stock_object =
      ANSI_FIXED_FONT |
      ANSI_VAR_FONT |
      BLACK_BRUSH |
      BLACK_PEN |
      DEFAULT_GUI_FONT |
      DEFAULT_PALETTE |
      DKGRAY_BRUSH |
      GRAY_BRUSH |
      HOLLOW_BRUSH |
      LTGRAY_BRUSH |
      NULL_BRUSH |
      NULL_PEN |
      OEM_FIXED_FONT |
      SYSTEM_FIXED_FONT |
      SYSTEM_FONT |
      WHITE_BRUSH |
      WHITE_PEN

    datatype rop_mode =
      BLACKNESS |
      DSTINVERT |
      MERGECOPY |
      MERGEPAINT |
      NOTSRCCOPY |
      NOTSRCERASE |
      PATCOPY |
      PATINVERT |
      PATPAINT |
      SRCAND |
      SRCCOPY |
      SRCERASE |
      SRCINVERT |
      SRCPAINT |
      WHITENESS
      
    val cancelDC : hdc -> unit (* only of use with native threads *) = env "nt cancel dc"
    val createCompatibleDC : hdc -> hdc = env "nt create compatible dc"
    val deleteObject : object -> unit = env "nt delete object"
    val getCurrentObject : hdc * object_type -> object = env "nt get current object"
    val getDC : hwnd -> hdc = env "nt get dc"
    val getDCOrgEx : hdc -> point = env "nt get dc org ex"
    val getStockObject : stock_object -> object = env "nt get stock object"
    val releaseDC : hwnd * hdc -> unit = env "nt release dc"
    val restoreDC : hdc * int -> unit = env "nt restore dc"
    val saveDC : hdc -> int = env "nt save dc"
    val selectObject : hdc * object -> object = env "nt select object"

    val bitBlt : 
       {hdcDest: hdc,
	hdcSrc:	 hdc,
	height:  int,
	ropMode: rop_mode,
	width:	 int,
	xDest:   int,
	xSrc:	 int,
	yDest:   int,
	ySrc:	 int} -> unit = env "win32 bit blt"

    datatype hatch_style =
      HS_BDIAGONAL
    | HS_CROSS
    | HS_DIAGCROSS
    | HS_FDIAGONAL
    | HS_HORIZONTAL
    | HS_VERTICAL
      
    val createHatchBrush : hatch_style * color -> hbrush = env "nt create hatch brush"
    val createSolidBrush : color -> hbrush = env "nt create solid brush"
(*
    val getBrushOrgEx : hdc -> point = env "nt get brush org ex"
    val setBrushOrg : hdc * point -> unit = env "nt set brush org"
*)

    datatype pen_style =
      PS_DASH |
      PS_DASHDOT |
      PS_DASHDOTDOT |
      PS_DOT |
      PS_NULL |
      PS_SOLID |
      PS_INSIDEFRAME

    (* Make a pen *)
    val createPen : pen_style * int * color -> hpen = env "nt create pen"

    (* Lines and curves *)
    datatype arc_direction = AD_CLOCKWISE| AD_COUNTERCLOCKWISE

    val angleArc : hdc * int * int * int * real * real -> unit = env "nt angle arc"
    val arc : hdc * int * int * int * int * int * int * int * int -> unit = env "nt arc"
    val arcTo : hdc * int * int * int * int * int * int * int * int -> unit = env "nt arc to"
    val getArcDirection : hdc -> arc_direction = env "nt get arc direction"
    val lineTo : hdc * int * int -> unit = env "nt line to"
    val moveTo : hdc * int * int * word -> unit = env "nt move to"
    val polyBezier : hdc * point list -> unit = unimpl "nt poly bezier"
    val polyBezierTo : hdc * point list -> unit = unimpl "nt poly bezier to"
    (* polyDraw needs some thought *)
    val polyline : hdc * point list -> unit = unimpl "nt polyline"
    val polylineTo : hdc * point list -> unit = unimpl "nt polyline to"
    val polyPolyline : hdc * point list list -> unit = unimpl "nt poly polyline"
    val setArcDirection : hdc * arc_direction -> unit = env "nt set arc direction"

    val chord : hdc * int * int * int * int * int * int * int * int -> unit = env "nt chord"
    val ellipse : hdc * int * int * int * int -> unit = env "nt ellipse"
    val fillRect : hdc * rect * hbrush -> unit = env "nt fill rect"
    val frameRect : hdc * rect * hbrush -> unit = env "nt frame rect"
    val invertRect : hdc * rect -> unit = env "nt invert rect"
    val pie : hdc * int * int * int * int * int * int * int * int -> unit = env "nt pie"
    val polygon : hdc * point list -> unit = unimpl "nt polygon"
    val polyPolygon : hdc * point list list -> unit = unimpl "nt poly polygon"
    val setPixel : hdc * int * int * color -> color = env "nt set pixel" 
    val rectangle : hdc * int * int * int * int -> unit = env "nt rectangle"
    val roundRect : hdc * int * int * int * int * int * int -> unit = env "nt round rect"
    val	textOut : hdc * int * int * string -> unit = env "nt text out"
    val getTextExtentPoint : hdc * string -> int * int = env "nt get text extent point"
    val validateRect : hwnd * rect option -> unit = env "nt validate rect"
    val invalidateRect : hwnd * rect option * bool -> unit = env "nt invalidate rect"
    val windowFromDC : hdc -> hwnd = env "nt window from dc"

    datatype rop2_mode =
      R2_BLACK |
      R2_COPYPEN |
      R2_MASKNOTPEN |
      R2_MASKPEN |
      R2_MASKPENNOT |
      R2_MERGENOTPEN |
      R2_MERGEPEN |
      R2_MERGEPENNOT |
      R2_NOP |
      R2_NOT |
      R2_NOTCOPYPEN |
      R2_NOTMASKPEN |
      R2_NOTMERGEPEN |
      R2_NOTXORPEN |
      R2_WHITE |
      R2_XORPEN

    val getRop2 : hdc -> rop2_mode = env "nt get rop2"
    val setRop2 : hdc * rop2_mode -> rop2_mode = env "nt set rop2"

    datatype bk_mode = 
      OPAQUE | 
      TRANSPARENT

    val getBkMode : hdc -> bk_mode = env "win32 get bk mode"
    val setBkMode : hdc * bk_mode -> bk_mode = env "win32 set bk mode"

    datatype color_spec =
      COLOR_ACTIVEBORDER |
      COLOR_ACTIVECAPTION |
      COLOR_APPWORKSPACE |
      COLOR_BACKGROUND |
      COLOR_BTNSHADOW |
      COLOR_BTNTEXT |
      COLOR_CAPTIONTEXT |
      COLOR_GRAYTEXT |
      COLOR_HIGHLIGHT |
      COLOR_HIGHLIGHTTEXT |
      COLOR_INACTIVEBORDER |
      COLOR_INACTIVECAPTION |
      COLOR_INACTIVECAPTIONTEXT |
      COLOR_MENU |
      COLOR_SCROLLBAR |
      COLOR_WINDOW |
      COLOR_WINDOWFRAME |
      COLOR_WINDOWTEXT

    val getSysColor : color_spec -> color = env "nt get sys color"
    val setBkColor : hdc * color -> color = env "nt set bk color"
    val getBkColor : hdc -> color = env "nt get bk color"
    val setTextColor : hdc * color -> color = env "nt set text color"
    val getTextColor : hdc -> color = env "nt get text color"

    (* Toolbars *)

    datatype tb_button_state = 
      TBSTATE_CHECKED |
      TBSTATE_ENABLED |
      TBSTATE_HIDDEN |
      TBSTATE_INDETERMINATE |
      TBSTATE_PRESSED |
      TBSTATE_WRAP

    val tbStatesToWord : tb_button_state list -> word = env "win32 convert tb states"

    datatype tb_button_style = 
      TBSTYLE_BUTTON |
      TBSTYLE_CHECK |
      TBSTYLE_CHECKGROUP |
      TBSTYLE_GROUP |
      TBSTYLE_SEP

    val createToolbarEx : 
	{bmp_id: word,
	 buttons: (int * word * tb_button_state list * tb_button_style list * word * int) list,
	 num_bmps: int,
	 num_buttons: int,
	 parent: hwnd,
	 styles: window_style list,
	 toolbar_id: word,
	 x_bitmap: int,
	 x_button: int,
	 y_bitmap: int,
	 y_button: int} ->
	hwnd = env "win32 create toolbar ex"

    (* Accelerator tables *)

    datatype accelerator_flag =
      FALT
    | FCONTROL
    | FNOINVERT
    | FSHIFT
    | FVIRTKEY

    (* First int is the key, the second is the command identifier *)
    val createAcceleratorTable : (accelerator_flag list * int * int) list -> accelerator_table =
      env "nt create accelerator table"

    val destroyAcceleratorTable : accelerator_table -> unit =
      env "nt destroy accelerator table"
    (* set the global mlworks accelerator table *)
    val setAcceleratorTable : accelerator_table -> unit = env "nt set accelerator table"

    (* clipboard *)

    val openClipboard : hwnd -> bool = env "nt open clipboard"
    val closeClipboard : unit -> unit = env "nt close clipboard"
    val emptyClipboard : unit -> unit = env "nt empty clipboard"
    val setClipboardData : string -> unit = env "nt set clipboard data"
    val getClipboardData : unit -> string = env "nt get clipboard data"

    (* MISC -- these should be somewhere else probably *)
    val malloc : int -> word = env "nt ml malloc"
    val free : word -> unit = env "nt ml free"
    val wordToString : word -> string = env "nt word to string"
    val setByte : word * int * int -> unit = env "nt set byte"
    val makeCString : string -> word = env "nt make c string"

    fun hiword w = wordToInt (Word.>> (w,0w16))
    fun loword w = wordToInt (Word.andb (w,intToWord ((256 * 256) - 1)))
  end
