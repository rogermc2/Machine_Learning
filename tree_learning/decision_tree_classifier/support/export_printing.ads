
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Export_Types;

package Export_Printing is

    Print_Error : Exception;

    procedure Print_Bounds (Name : String; Data : Export_Types.Bounds_List);
    procedure Print_Colours_List (Name    : String;
                                  Colours : Export_Types.Colours_List);
    procedure Print_Integer_Colours_List
      (Name : String; Colours : Export_Types.Integer_Colours_List);
    procedure Print_Export_Map (Name : String; aMap : Export_Types.Export_Map);
    procedure Print_RGB_Array (Name : String; anArray : Export_Types.RGB_Array);

end Export_Printing;
