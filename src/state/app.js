const APP_RESET_ACTION = 'APP_RESET_ACTION';
const APP_RESIZE_ACTION = 'APP_RESIZE_ACTION';
const APP_MUTE_ACTION = 'APP_MUTE_ACTION';
const APP_UNMUTE_ACTION = 'APP_UNMUTE_ACTION';
const AUDIO_SOURCE_ADD_ACTION = 'AUDIO_SOURCE_ADD_ACTION';
const AUDIO_SOURCE_REMOVE_ACTION = 'AUDIO_SOURCE_REMOVE_ACTION';
const AUDIO_SOURCE_MUTE_ACTION = 'AUDIO_SOURCE_MUTE_ACTION';
const AUDIO_SOURCE_UNMUTE_ACTION = 'AUDIO_SOURCE_UNMUTE_ACTION';
const AUDIO_SOURCE_MOVE_ACTION = 'AUDIO_SOURCE_MOVE_ACTION';

const genAction = (action, value) => ({
  type: action,
  value: value
});

export const appReset = value => genAction(APP_RESET_ACTION, value);
export const appResize = value => genAction(APP_RESIZE_ACTION, value);
export const appMute = value => genAction(APP_MUTE_ACTION, value);
export const appUnmute = value => genAction(APP_UNMUTE_ACTION, value);
export const audioSourceAdd = value => genAction(AUDIO_SOURCE_ADD_ACTION, value);
export const audioSourceRemove = value => genAction(AUDIO_SOURCE_REMOVE_ACTION, value);
export const audioSourceMute = value => genAction(AUDIO_SOURCE_MUTE_ACTION, value);
export const audioSourceUnmute = value => genAction(AUDIO_SOURCE_UNMUTE_ACTION, value);
export const audioSourceMove = value => genAction(AUDIO_SOURCE_MOVE_ACTION, value);

const initialState = {
  size: {
    width: 0,
    height: 0
  },
  audioSources: []
};

export const appReducer = (state = initialState, action) => {
  switch (action.type) {
    case APP_RESET_ACTION:
      return initialState;
    case APP_RESIZE_ACTION:
      return Object.assign({}, state, {size: action.value})
    case APP_MUTE_ACTION:
      break;
    case APP_UNMUTE_ACTION:
      break;
    case AUDIO_SOURCE_ADD_ACTION:
      break;
    case AUDIO_SOURCE_REMOVE_ACTION:
      break;
    case AUDIO_SOURCE_MUTE_ACTION:
      break;
    case AUDIO_SOURCE_UNMUTE_ACTION:
      break;
    case AUDIO_SOURCE_MOVE_ACTION:
      break;
    default:
      return state;
  }
};