--  Ragnvaldr Systems -- Raytracer
--
--  Copyright (C) 2019  Ronald van Manen
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
