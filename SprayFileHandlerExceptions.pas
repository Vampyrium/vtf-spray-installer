unit SprayFileHandlerExceptions;

interface

const
  SFH_E_LAYOUT_OUTSIDE: string = 'Line %d: %s declaration outside file layout definition';
  SFH_E_GAME_OUTSIDE: string = 'Line %d: %s declaration outside game definition';
  SFH_E_TOO_MANY_ARGS: string = 'Line %d: too many arguments for %s';
  SFH_E_GAME_UNDEFINED_LAYOUT: string = 'Line %d: File layout %s does not exist';

implementation

end.
