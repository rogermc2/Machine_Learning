
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities; use Classifier_Utilities;
with Models; use Models;

procedure Test_Functions is
   X      : constant Float_Array (1 .. 6) := (0.0, 1.0, 2.0, 3.0, 4.0, 5.0);
   B      : constant Float_Array (1 .. 4) := (13.7, 1.0, 1.0, 9.7);
   B_List : constant Float_List := To_Float_List (B);
   X_List : constant Float_List := To_Float_List (X);
        Y : constant Float_List := Linear_Function (B_List, X_List);
begin
   Print_Float_List ("Y", Y);
end Test_Functions;
