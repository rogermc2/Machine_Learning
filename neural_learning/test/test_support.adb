
with Ada.Text_IO; use Ada.Text_IO;

package body Test_Support is

    procedure Print_Float_Matrix
      (Name  : String; aMatrix : Real_Float_Matrix) is
    begin
        Put_Line (Name & ": ");
        for row in aMatrix'Range loop
            for col in aMatrix'Range (2) loop
                Put (Float'Image (aMatrix (row, col)) & "  ");
            end loop;
            New_Line;
        end loop;

    end Print_Float_Matrix;

    --  ------------------------------------------------------------------------

end Test_Support;
