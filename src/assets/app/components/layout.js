import React from 'react';
import styled from 'styled-components';

import Container from '@material-ui/core/Container';
import Footer from '@arcblock/ux/lib/Footer';

export default function Layout({ children }) {
  return (
    <Wrapper maxWidth="md" style={{ maxWidth: 800 }}>
      <div className="header">Header</div>
      <div className="content">{children}</div>
      <Footer style={{ margin: 0 }} className="footer"></Footer>
    </Wrapper>
  );
}

const Wrapper = styled(Container)`
  min-height: 60vh;
  border: 1px solid ${props => props.theme.colors.minor};
  && {
    padding: 24px;
  }

  .content {
  }

  .footer {
    padding-top: 8px;
  }
`;
