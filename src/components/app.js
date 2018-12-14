import React, { Component } from 'react';
import { combineReducers, createStore } from 'redux';
import { Provider, connect } from 'react-redux';
import devToolsEnhancer from 'remote-redux-devtools';
import {
  appReducer,
  appReset,
  appResize,
  appMute,
  appUnmute,
  audioSourceAdd,
  audioSourceRemove,
  audioSourceMute,
  audioSourceUnmute,
  audioSourceMove
} from '../state/app';

class LocalApp extends Component {
  // constructor(props) {
  //   super(props);
  //   this.state = {
  //     width: window.innerWidth,
  //     height: window.innerHeight
  //   }
  // }

  componentWillMount = () => {
    this.onWindowResize();
  }

  componentDidMount = () => {
    window.addEventListener('resize', this.onWindowResize);
  }

  componentWillUnmount = () => {
    window.removeEventListener('resize', this.onWindowResize);
  }
  
  onWindowResize = () => {
    this.props.appResize({      
      width: window.innerWidth,
      height: window.innerHeight
    });

    // this.setState({
    //   width: window.innerWidth,
    //   height: window.innerHeight
    // });
  }

  render = () => (<div>
    <div>width: {this.props.app.size.width} </div>
    <div>height: {this.props.app.size.height} </div>    
  </div>)
}

//==============================================================================
// REDUX
//==============================================================================

const mapStateToProps = state => ({
  app: appReducer(state, '')
});

const mapStateToActions = dispatch => ({
  appReset: value => dispatch(appReset(value)),
  appResize: value => dispatch(appResize(value)),
  appMute: value => dispatch(appMute(value)),
  appUnmute: value => dispatch(appUnmute(value)),
  audioSourceAdd: value => dispatch(audioSourceAdd(value)),
  audioSourceRemove: value => dispatch(audioSourceRemove(value)),
  audioSourceMute: value => dispatch(audioSourceMute(value)),
  audioSourceUnmute: value => dispatch(audioSourceUnmute(value)),
  audioSourceMove: value => dispatch(audioSourceMove(value))
});

const _App = connect (
  mapStateToProps,
  mapStateToActions
)(LocalApp);

// let reducers = combineReducers({
//   appReducer
// });

const store = createStore(appReducer, devToolsEnhancer());

const App = props => {
  return (
    <React.Fragment>
      <Provider store={store}>
        <_App/>
      </Provider>
    </React.Fragment>
  )
}

export {
  App
}