import React from 'react';
import styled from 'styled-components';

import Container from '@material-ui/core/Container';

export default function Layout({ children }) {
  console.log({ children });
  return <Wrapper maxWidth="sm">{children}</Wrapper>;
}

const Wrapper = styled(Container)``;
