import React from 'react';
import CssBaseline from '@material-ui/core/CssBaseline';
import { BrowserRouter as Router, Route, Switch, Redirect } from 'react-router-dom';
import { create } from '@arcblock/ux/lib/Theme';
import { MuiThemeProvider } from '@material-ui/core/styles';
import { createGlobalStyle, ThemeProvider } from 'styled-components';

import Layout from './components/layout';
import PageDummy from './pages/dummy';

const theme = create();
const GlobalStyle = createGlobalStyle``;

export default function App() {
  return (
    <MuiThemeProvider theme={theme}>
      <ThemeProvider theme={theme}>
        <React.Fragment>
          <CssBaseline />
          <GlobalStyle />
          <Router>
            <Layout>
              <Switch>
                <Route exact path="/app/dummy" component={PageDummy} />
                <Redirect from="/app" to="/app/dummy" />
              </Switch>
            </Layout>
          </Router>
        </React.Fragment>
      </ThemeProvider>
    </MuiThemeProvider>
  );
}
