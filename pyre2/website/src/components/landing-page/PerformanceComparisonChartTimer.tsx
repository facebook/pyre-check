/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import React, { useState, useEffect } from 'react';

interface TimerProps {
    targetSeconds: number;
}

const PerformanceComparisonChartTimer: React.FC<TimerProps> = ({ targetSeconds }) => {
    const [seconds, setSeconds] = useState(0.0);

    useEffect(() => {
        if (seconds <= targetSeconds) {
            const interval = setInterval(() => {
                setSeconds(prevSeconds => prevSeconds + 1);
            }, 1000);

            return () => clearInterval(interval);
        }
    }, [seconds, targetSeconds]);

    return (
        <strong>{`${seconds}s`}</strong>
    );
};

export default PerformanceComparisonChartTimer;
