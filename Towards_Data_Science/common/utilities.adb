
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

package body Utilities is

    procedure Print_Raw_Question (Self : ML_Types.Raw_Question) is
    use ML_Types;
    --  Example" Self = ("Colour", "Green"));
        Col   : constant String := To_String (Self.Feature_Name);
        Value : constant String := To_String (Self.Feature_Value);
    begin
        Put_Line ("Raw_Question: Is " & Col & " = " & " " & Value);
    end Print_Raw_Question;

    --  --------------------------------------------------------------------------

end Utilities;
