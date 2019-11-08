import React from 'react';
import styled from 'styled-components';

import Container from '@material-ui/core/Container';
import Footer from '@arcblock/ux/lib/Footer';
import useWindowSize from 'react-use/lib/useWindowSize';

export default function Layout({ children }) {
  const { width } = useWindowSize();
  const baseWidth = width > 800 ? 800 : width;

  return (
    <Wrapper maxWidth="md" style={{ maxWidth: 800 }} baseWidth={baseWidth}>
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
  border: 1px solid ${props => props.theme.colors.lightGrey};
  && {
    padding: 0;
  }

  .header-container {
    height: 120px;
    position: relative;
    overflow: hidden;
    z-index: -1;

    .header-bg-light {
      position: absolute;
      left: -${props => props.baseWidth * 3 - props.baseWidth / 2}px;
      bottom: 0px;
      width: ${props => props.baseWidth * 6}px;
      height: ${props => props.baseWidth * 6}px;
      border-radius: ${props => props.baseWidth * 3}px;
      background-color: #a2b1f7;
    }

    .header-bg-dark {
      position: absolute;
      left: -${props => (props.baseWidth * 3 - props.baseWidth / 2) * 0.95}px;
      bottom: 2px;
      width: ${props => props.baseWidth * 6}px;
      height: ${props => props.baseWidth * 6}px;
      border-radius: ${props => props.baseWidth * 3}px;
      background-color: ${props => props.theme.colors.blue};
    }
  }

  .content-wrapper {
    margin: -106px 40px 40px;
    z-index: 99;
    @media (max-width: 768px) {
      margin: -106px 24px 24px;
    }
  }

  .footer-container {
    margin: 24px;
    padding-top: 0;
    padding-bottom: 0;
    width: auto;
  }
`;
