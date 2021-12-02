
with Ada.Text_IO; use Ada.Text_IO;

with Classification;

procedure Iris is

begin
   Put_Line ("Iris started");
   Classification.Classify_Iris;

end Iris;
