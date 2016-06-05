/// Module with FParsec parser for ISD data.
module ISD.Parser

open FParsec
open System
open System.IO

/// Data entry for ISD data.
type ISDRecord = 
    {
        USAFMasterStationCatalog       : string 
        NCDCWbanIdentifier             : string
        Date                           : int
        Time                           : int
        DataSource                     : char
        Latitude                       : float
        Longitude                      : float
        ReportType                     : string
        Elevation                      : float
        CallLetterIdentifier           : string
        QualityControlProcess          : string
        WindDirection                  : Option<float>
        WindSpeedMetersPerSec          : Option<float>     
        CeilingHeightMeters            : Option<float>
        VisibilityMeters               : Option<float>
        AirTemperatureCelsius          : Option<float>
        DewPointCelsius                : Option<float>
        SeaLevelPressureHectopascals   : Option<float>
        WindDirectionQC                : char
        WindSpeedMetersPerSecQC        : char     
        CeilingHeightMetersQC          : char
        VisibilityMetersQC             : char
        AirTemperatureCelsiusQC        : char        
        DewPointCelsiusQC              : char
        SeaLevelPressureHectopascalsQC : char 
    }

    static member Default = {
        USAFMasterStationCatalog        = ""
        NCDCWbanIdentifier              = ""
        Date                            = 0
        Time                            = 0
        DataSource                      = ' '
        Latitude                        = nan
        Longitude                       = nan
        ReportType                      = ""
        Elevation                       = nan
        CallLetterIdentifier            = ""
        QualityControlProcess           = ""
        WindDirection                   = None
        WindSpeedMetersPerSec           = None     
        CeilingHeightMeters             = None
        VisibilityMeters                = None
        AirTemperatureCelsius           = None
        DewPointCelsius                 = None
        SeaLevelPressureHectopascals    = None
        WindDirectionQC                 = ' '
        WindSpeedMetersPerSecQC         = ' '     
        CeilingHeightMetersQC           = ' '
        VisibilityMetersQC              = ' '
        AirTemperatureCelsiusQC         = ' '        
        DewPointCelsiusQC               = ' '
        SeaLevelPressureHectopascalsQC  = ' ' }

/// Parser returning `None`when condition holds.
let noneIf (cond : 'v -> bool) (p : Parser<'v,'u>) : Parser<Option<'v>,'u> =
    p |>> (fun v -> if cond v then None else Some v)

/// Parser reading a string of `n` characters.
let nChars (n : int) : Parser<string,'u> = 
    parray n anyChar |>> (fun s -> System.String s)

/// Parser reading an integer with sign and `n` digits.
let nSignedInt (n : int) : Parser<int,'u> = 
    (pchar '+' >>% (fun x -> x)) <|> (pchar '-' >>% (fun x -> -x)) >>=
    (fun s -> nChars n |>> (fun v -> s (int v)))

/// Parser reading an unsigned integer with `n` digits.
let nUnsignedInt (n : int) : Parser<int,'u> = 
    nChars n |>> (fun v -> int v)

/// Parser signed number into float. `n` digits and scale `scale`. 
let nSignedFloat (n : int) (scale : float) : Parser<float,'u> = 
    nSignedInt n |>> (fun v -> scale * (float v))

/// Parser unsigned number into float. `n` digits and scale `scale`. 
let nUnsignedFloat (n : int) (scale : float) : Parser<float,'u> = 
    nUnsignedInt n |>> (fun v -> scale * (float v))

/// Combine parser with action.
let (>>>) (lhs : Parser<'v,'u>) (rhs : 'r -> 'v -> 'r) : 'r -> Parser<'r,'u> =
    fun r -> lhs |>> rhs r

/// Parser reading a line into a data record.
let parseEntry : Parser<ISDRecord,unit> =
    preturn ISDRecord.Default .>>
    nChars 4 >>=
    (nChars 6                                            >>> (fun r v -> { r with USAFMasterStationCatalog = v })) >>=
    (nChars 5                                            >>> (fun r v -> { r with NCDCWbanIdentifier = v })) >>=
    (nChars 8                                            >>> (fun r v -> { r with Date = int v })) >>=
    (nChars 4                                            >>> (fun r v -> { r with Time = int v })) >>=
    (anyChar                                             >>> (fun r v -> { r with DataSource = v })) >>=
    (nSignedFloat 5 (1.0 / 1000.0)                       >>> (fun r v -> { r with Latitude = v })) >>=
    (nSignedFloat 6 (1.0 / 1000.0)                       >>> (fun r v -> { r with Longitude = v })) >>=
    (nChars 5                                            >>> (fun r v -> { r with ReportType = v })) >>=
    (nSignedFloat 4 1.0                                  >>> (fun r v -> { r with Elevation = v })) >>=
    (nChars 5                                            >>> (fun r v -> { r with CallLetterIdentifier = v })) >>=
    (nChars 4                                            >>> (fun r v -> { r with QualityControlProcess = v })) >>=
    (noneIf ((<=) 360.) (nUnsignedFloat 3 1.0)           >>> (fun r v -> { r with WindDirection = v})) >>=
    (anyChar                                             >>> (fun r v -> { r with WindDirectionQC = v})) >>=
    (anyChar                                             >>> (fun r _ -> r)) >>=        
    (noneIf ((<=) 999.) (nUnsignedFloat 4 10.0)          >>> (fun r v -> { r with WindSpeedMetersPerSec = v})) >>=
    (anyChar                                             >>> (fun r v -> { r with WindSpeedMetersPerSecQC = v})) >>=
    (noneIf ((<=) 99999.) (nUnsignedFloat 5 1.0)         >>> (fun r v -> { r with CeilingHeightMeters = v})) >>=
    (anyChar                                             >>> (fun r v -> { r with CeilingHeightMetersQC = v})) >>=
    (anyChar                                             >>> (fun r _ -> r)) >>=        
    (anyChar                                             >>> (fun r _ -> r)) >>=        
    (noneIf ((<=) 999999.) (nUnsignedFloat 6 1.0)        >>> (fun r v -> { r with VisibilityMeters = v})) >>=
    (anyChar                                             >>> (fun r v -> { r with VisibilityMetersQC = v})) >>=
    (anyChar                                             >>> (fun r _ -> r)) >>=        
    (anyChar                                             >>> (fun r _ -> r)) >>=        
    (noneIf ((<=) 999.) (nSignedFloat 4 (1.0 / 10.0))    >>> (fun r v -> { r with AirTemperatureCelsius = v })) >>=
    (anyChar                                             >>> (fun r v -> { r with AirTemperatureCelsiusQC = v })) >>=
    (noneIf ((<=) 999.) (nSignedFloat 4 (1.0 / 10.0))    >>> (fun r v -> { r with DewPointCelsius = v })) >>=
    (anyChar                                             >>> (fun r v -> { r with DewPointCelsiusQC = v })) >>=
    (noneIf ((<=) 9999.) (nUnsignedFloat 5 (1.0 / 10.0)) >>> (fun r v -> { r with SeaLevelPressureHectopascals = v })) >>=
    (anyChar                                             >>> (fun r v -> { r with SeaLevelPressureHectopascalsQC = v })) .>>
    skipRestOfLine true

/// Parse line into `ISDRecord`.
let parseLine (str : string) : ISDRecord = 
    match run parseEntry str with
    | ParserResult.Success(r,_,_) -> r
    | ParserResult.Failure(e,_,_) -> failwithf "Parser failure %A" e

/// Read lines of file into sequence.
let getLinesOfFile (filePath : string) = 
    seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }
