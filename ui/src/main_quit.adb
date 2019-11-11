with Gtk.Main;
with Gtk.Widget;

procedure Main_Quit (Self : access Gtk.Widget.Gtk_Widget_Record'Class) is
begin
    Gtk.Main.Main_Quit;
end Main_Quit;
