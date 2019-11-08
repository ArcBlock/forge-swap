import React from 'react';
import styled from 'styled-components';

import Container from '@material-ui/core/Container';
import Footer from '@arcblock/ux/lib/Footer';

export default function Layout({ children }) {
  return (
    <Wrapper maxWidth="md" style={{ maxWidth: 800 }}>
      <div className="header-container">
        <div className="header-bg-light"></div>
        <div className="header-bg-dark"></div>
      </div>
      <div className="content-wrapper">{children}</div>
      <Footer className="footer-container"></Footer>
    </Wrapper>
  );
}

const Wrapper = styled(Container)`
  position: relative;
  min-height: 80vh;
  border: 1px solid ${props => props.theme.colors.lightGrey};
  && {
    padding: 0;
  }

  .header-container {
    height: 120px;
    position: relative;
    overflow: hidden;

    .header-bg-light {
      position: absolute;
      left: -2000px;
      bottom: 0px;
      width: 4800px;
      height: 4800px;
      border-radius: 2400px;
      background-color: #A2B1F7;
    }

    .header-bg-dark {
      position: absolute;
      left: -1950px;
      bottom: 2px;
      width: 4800px;
      height: 4800px;
      border-radius: 2400px;
      background-color: ${props => props.theme.colors.blue};
    }
  }

  .content-wrapper {
    margin: 24px;
    position: absolute;
    top: 0;
    left: 0;
  }

  .footer-container {
    margin: 24px;
    padding-top: 0;
    padding-bottom: 0;
    width: auto;
  }
`;
