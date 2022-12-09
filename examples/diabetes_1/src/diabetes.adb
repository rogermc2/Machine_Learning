
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with ML_Types;
with Builder;

procedure Diabetes is
   use ML_Types;
   Data_File      : File_Type;
   Training_Data  : ML_Types.Rows_Vector;
   aTree          : Tree_Type;
   aRow           : Row_Data;
begin
   Put_Line ("Diabetes Example");
   Open (Data_File, In_File, "../diabetes.csv");
   Utilities.Load_CSV_Data (Data_File, Training_Data);
   Close (Data_File);
   Put_Line ("Build Tree");
   aTree := Builder.Build_Tree (Training_Data, 3);
   New_Line;

   Put_Line ("Print Tree");
   Utilities.Print_Tree (aTree);
   New_Line;

   aRow := Training_Data.Element (16);
   Utilities.Print_Row ("Row 16", aRow);

end Diabetes;
