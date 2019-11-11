with Gtk.Box;          use Gtk.Box;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Main;
with Gtk.Window;       use Gtk.Window;

with Main_Quit;

procedure Main is
    MainWindow   : Gtk_Window;
    Drawing_Area : Gtk_Drawing_Area;
begin
    Gtk.Main.Init;

    Gtk_New (MainWindow);
    MainWindow.Set_Default_Size (800, 600);
    MainWindow.On_Destroy (Main_Quit'Access);

    Gtk_New (Drawing_Area);
    Drawing_Area.Set_Size_Request(640, 480);
    MainWindow.Add(Drawing_Area);

    --  Show the window and present it
    MainWindow.Show_All;
    MainWindow.Present;

    Gtk.Main.Main;
end Main;
