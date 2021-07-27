
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with ML_Types;
with Builder;

procedure Diabetes is
    use ML_Types;

    Data_File      : File_Type;
    Training_Data  : ML_Types.Rows_Vector;
    aTree          : Tree_Type;
begin
    Put_Line ("Diabetes Example");
    Open (Data_File, In_File, "src/diabetes.csv");
    begin
        Utilities.Load_CSV_Data (Data_File, Training_Data);
        Close (Data_File);
        Put_Line ("Build Tree");
        aTree := Builder.Build_Tree (Training_Data);
        New_Line;

        Put_Line ("Print Tree");
        Utilities.Print_Tree (aTree);
        New_Line;
    end;

end Diabetes;
