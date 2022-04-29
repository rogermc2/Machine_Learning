
with Ada.Text_IO; use Ada.Text_IO;

with Export_Tests;

procedure Test_Export is

begin
   Put_Line ("Test Export started");
   Export_Tests.Test_Graphviz_Toy;

end Test_Export;
